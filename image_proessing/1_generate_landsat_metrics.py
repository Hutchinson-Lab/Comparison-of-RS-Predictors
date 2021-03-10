import ee
from os import system
from time import sleep

ee.Initialize()

class GenerateSeasonalMetrics():
    
    # Define the julian days
    __spring_start_day = 105
    __spring_end_day = 196
    __summer_start_day = 166
    __summer_end_day = 259
    __fall_start_day = 227
    __fall_end_day = 319
    
    def __init__(self, start_year, end_year, start_export_year, end_export_year, study_area_geo, project_name, username):
        
        # Set the class attributes
        self.__start_year = start_year
        self.__end_year = end_year
        self.__start_export_year = start_export_year
        self.__end_export_year = end_export_year
        self.__username = username
        self.__project_name = project_name
        self.__study_area_geo = study_area_geo
                
        # Define the task dictionary
        self.__covariate_export_tasks = {}
        
        # Generate the folders required for image processing
        #self.__make_project_directory()
        
        return None
        
    def generate_metrics(self):
        '''
        The primary method of this object. Following instnatiation, running this function will:
            1. Generate the spring, summer, and fall image collections over the input study area
            2. Generate annual medoid composites using each image collection
            3. Interpolate each of the annual medoid time-series using LandTrendr to obtain
               gap-free annual mosaics. Four instances of interpolation are performed:
                   - The spring time-series is interpolated using the NBR band
                   - The summer time-series is interpolated using the NBR band
                   - The summer time-series is interpolated using the B5 band
                   - The fall time-series is interpolated using the NBR band
            4. Export the interpolated gap-free mosaics to the input users' GEE Assets folder
            
        Return:
            None
        '''
        # Load in the seasonal image collections
        seasonal_collections = self.__load_image_collections()
        
        # Compute the medoid composites
        medoid_collections = []
        for image_collection in seasonal_collections:
            medoid_collections.append(self.__create_annual_composites(image_collection))
            
        # Export each of the interpolated time-series to Google Earth Assets
        interpolated_colllections = []
        segmentation_params = [['NBR', 0], ['NBR', 1], ['B5', 1], ['NBR', 2]]
        for sub_list in segmentation_params:
            
            # Get the correct image collection using the index in the sublist
            medoid_images = ee.ImageCollection(medoid_collections[sub_list[1]])
            
            # Prep the image collection for segmentation
            if sub_list[0] == 'NBR':
                medoid_images = medoid_images.map(self.__add_seg_band_nbr)
            else:
                medoid_images = medoid_images.map(self.__add_seg_band_b5)
                
            # Get the LandTrendr Fitted Values
            interpolated_colllections.append(self.__interpolate_time_series(medoid_images))
            
        # Export the image collections to their asset collections
        asset_names = ['spring_nbr', 'summer_nbr', 'summer_b5', 'fall_nbr']
        for i, collection in enumerate(interpolated_colllections):
            
            if asset_names[i] != "summer_nbr":
                pass
            else:
                self.__export_fitted_composites(collection, asset_names[i])
                
        return None
            
    def __make_project_directory(self):
        '''
        This functions generates an asset directory (using the __project_name attribute)
        and then generates image collections within the project asset directory to 
        store the fitted composite images
        
        Returns:
            None
        '''
        # Generate the root folder for the project
        system('earthengine create folder users/'+self.__username+'/'+self.__project_name)
        
        # Generate the sub-directories
        for sub_dir in ['spring_nbr','summer_nbr','summer_b5','fall_nbr']:
            system('earthengine create collection  users/'+self.__username+'/'+self.__project_name+'/'+sub_dir)
        
        return
    
    def __load_image_collections(self):
        '''
        Loads 3 ee.ImageCollections, for the spring, summer, and fall seasons, of Landsat imagery
        over the study area defined when the GenerateSeasonalMetrics obj was instantiated. 
        
        Returns:
            List - contains 3 Image Collections (one for each: spring, summer, fall imagery)
        '''
        # Define a list of the start and end days
        julian_days = [[self.__spring_start_day, self.__spring_end_day], 
                       [self.__summer_start_day, self.__summer_end_day],
                       [self.__fall_start_day, self.__fall_end_day]]
        
        # Load in the landsat imagery
        outputs = []
        for sub_list in julian_days:
            seasonal_collection = self.__load_landsat_imagery(sub_list[0], sub_list[1])
            outputs.append(seasonal_collection)
        
        return outputs
    
    def __load_landsat_imagery(self, start_day, end_day):
        '''
        Loads a time-series of Landsat 4-8 imagery, filtered based on the supplied
        dates and the geometry. The Landsat 4, 5, and 7 imagery are harmonized to the 
        the OLI sensor
        
        Parameters:
            start_day (int): Defines the first Julain day of each year
            end_day (int): Defines the end Julain day of each year
        
        Returns:
            ee.ImageCollection The time-series of landsat imagery
        
        '''        
        #Construct the ee.Dates to be used when filtering the time-series
        start_date = ee.Date.fromYMD(self.__start_year, 1, 1)
        end_date = ee.Date.fromYMD(self.__end_year, 12, 31)
        
        #Load the Landsat Imagery
        landsat_5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR') \
            .filterDate(start_date, end_date) \
            .filter(ee.Filter.calendarRange(start_day, end_day)) \
            .filterBounds(self.__study_area_geo) \
            .map(self.__apply_pixel_qa) \
            .map(self.__tm_etm_to_oli)
        landsat_7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR') \
            .filterDate(start_date, end_date) \
            .filter(ee.Filter.calendarRange(start_day, end_day)) \
            .filterBounds(self.__study_area_geo) \
            .map(self.__apply_pixel_qa) \
            .map(self.__tm_etm_to_oli)
        landsat_8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR') \
            .filterDate(start_date, end_date) \
            .filter(ee.Filter.calendarRange(start_day, end_day)) \
            .filterBounds(self.__study_area_geo) \
            .select(['B2','B3','B4','B5','B6','B7','pixel_qa'],['B1','B2','B3','B4','B5','B7','pixel_qa']) \
            .map(self.__apply_pixel_qa)
        
        return ee.ImageCollection(landsat_8.merge(landsat_7).merge(landsat_5))
    
    def __tm_etm_to_oli(self, tm_or_etm):
        '''
        Applies a linear transformation to each band to harmonize the spectral values
        of each input TM or ETM+ image to the OLI based on coefficents from David Roy's 2016 paper:
        "Characterization of Landsat-7 to Landsat-8 reflective wavelength and 
        normalized difference vegetation index continuity"
        
        Parameters:
            tm_or_etm (ee.Image): A 6-band TM or ETM+ Landsat image. 
        
        Returns:
            ee.Image The cross-harmonized TM or ETM+ Landsat image.
          
        '''
        #Roy et al. 2016 - Table 2. RMA ETM+ to OLI slope coefficents
        slopes = ee.Image.constant([0.9785, 0.9542, 0.9825, 1.0073, 1.0171, 0.9949 ])
        
        #Roy et al. 2016 - Table 2. RMA ETM+ to OLI slope coefficents
        intercepts = ee.Image.constant([-0.0095, -0.0016, -0.0022, -0.0021, -0.0030, 0.0029])
        
        transformed = tm_or_etm.select(['B1','B2','B3','B4','B5','B7']) \
            .resample('bicubic') \
            .subtract(intercepts.multiply(10000)) \
            .divide(slopes)
               
        return transformed.set('system:time_start', tm_or_etm.get('system:time_start')).toInt16()  

    def __apply_pixel_qa(self, image):
        '''
        Applies the pixel_qa layer produced by FMASK to a Landsat scene.
        
        Parameters:
            image (ee.Image): A Landsat image with the included "pixel_qa" band 
        
        Returns:
            ee.Image The Landsat image with the included masked applied. The returned images 
                     will drop the pixel_qa band.
        
        '''
        qa = image.select('pixel_qa')
        mask = qa.bitwiseAnd(1 << 3).eq(0) \
            .And(qa.bitwiseAnd(1 << 4).eq(0)) \
            .And(qa.bitwiseAnd(1 << 5).eq(0))
        return image.updateMask(mask).select(['B1','B2','B3','B4','B5','B7']) 

    def __create_annual_composites(self, image_collection):
        '''
        Convert's a time-series of images into a time-series of annual medoid composites.
        
        image_collection (ee.ImageCollection)  The time-series of imagery.
         
        Returns:
            ee.ImageCollection The collection of annual medoid composites 
        
        '''
        #Loop through the years and generate the time-series of composited images
        output = []
        for year in range(self.__start_year, self.__end_year+1):
        
            #Generate the start and end dates to filter the time-series
            start_date = ee.Date.fromYMD(year, 1, 1)
            end_date = ee.Date.fromYMD(year, 12, 31)
            
            #Create the composited image for a given year
            composite = self.__landsat_medoid_mosaic(image_collection.filterDate(start_date, end_date)) \
                .set('system:time_start', ee.Date.fromYMD(year, 8, 1).millis()) \
                .toInt16()
            
            #Append the new image to the JavaScript array
            output.append(composite)
        
        return ee.ImageCollection.fromImages(output)
    
    def __landsat_medoid_mosaic(self, input_collection):
        '''
        Takes a collection of images and produces a medoid composite. Medoid: "Medoids are 
        representative objects of a data set or a cluster with a data set whose average 
        dissimilarity to all the objects in the cluster is minimal." -Wikipedia.
        
        Parameters:
            input_collection (ee.ImageCollection): The image collection to be composited.
        
        Returns:
            ee.Image The medoid composited image
        
        '''
        #Add a dummy image to prevent issues with an empty collection (years with no valid observations)
        dummy = ee.ImageCollection([ee.Image([0,0,0,0,0,0]).mask(ee.Image(0))]) \
            .select([0,1,2,3,4,5],['B1','B2','B3','B4','B5','B7'])
        images_to_composite = ee.ImageCollection(input_collection.merge(dummy)) 
        
        #Compute the median median across images in collection per band
        median = images_to_composite.median()                                                                       
        
        #Calculate the distance of each observation from the median
        def inner_median_diff (img):
            diff = ee.Image(img).subtract(median).pow(2)                                       
            return diff.reduce(ee.Reducer.sum()).addBands(img)                                                                   
        median_difference = images_to_composite.map(inner_median_diff)
        
        #Select the image with the smallest difference from the median
        return ee.ImageCollection(median_difference) \
            .reduce(ee.Reducer.min(7)) \
            .select([1, 2, 3, 4, 5, 6], ['B1','B2','B3','B4','B5','B7']) 
        
    def __add_seg_band_nbr(self, image):
        '''
        Computes the Normalized Burn Ratio for a medoid composited Landsat image.
        The NBR values are inverted so that they can be used with the LandTrendr algorithm.
        The NBR band is set as the first band in the image.
        
        Parameters:
            image (ee.Image): A medoid composited landsat image with the 6 default bands. 
         
        Returns:
            ee.Image An image with the inverted NBR band as the first band
        
        '''
        #Compute the inverted NBR band
        nbr = image.normalizedDifference(['B4','B7']).rename(['NBR_SEG']).multiply(-1)
        
        return nbr.addBands(image).set('system:time_start', image.get('system:time_start')).float()
    
    def __add_seg_band_b5(self, image):
        '''
        Adds an additional B5 as the first band to a given image. Functions assumes the input 
        image has all of the default bands. 
        
        Parameters:
            image (ee.Image): A medoid composited landsat image with the 6 default bands. 
         
        Returns:
            ee.Image An image with the inverted NBR band as the first band
        
        '''
        #Compute the inverted NBR band
        b5 = image.select(['B5']).rename(['B5_SEG'])
        
        return b5.addBands(image).set('system:time_start', image.get('system:time_start')).float()

    def __interpolate_time_series(self, image_collection):
        '''
        Summary. This function processes the LandTrendr output into a time-series of interpolated images.
        This function assumes that the LandTrendr output has 6 1D array images produced by the 
        fitting stage of the LandTrendr algorithm: B1, B2, B3, B4, B5, B7
        The band used to segment the time-series does not matter.
        
        Parameters:
            image_collection (ee.ImageCollection): An time-series of annual composited images (must be 1 image for each year 
                                                   of the time-series). The first band will be used for the segmentation proceedure
                                                   the remaining bands will be be fitted via the segmentation results. This function
                                                   feeds into a flattening script and assumes that bands: B1, B2, B3, B4, B5, B7
                                                   exist for each image in the image collection. 
        Returns:
            ee.Image The output of the Earth Engine LandTrendr implementation
        
        '''
        # Run the LandTrendr algorithm
        ltr_output = ee.Algorithms.TemporalSegmentation.LandTrendr(
            timeSeries = image_collection,
            maxSegments = 9,
            spikeThreshold = 0.9,
            vertexCountOvershoot = 3,
            preventOneYearRecovery = True,
            recoveryThreshold = 0.25,
            pvalThreshold = 0.05,
            bestModelProportion = 0.75,
            minObservationsNeeded = 6
            )
        
        # Flatten the LandTrendr outputs
        flattend_images = self.__flatten_ltr_fits(ltr_output)
        
        return flattend_images
    
    def __flatten_ltr_fits(self, ltr_output):
        '''
        This function processes the LandTrendr output into a time-series of interpolated images.
        This function assumes that the LandTrendr output has 6 1D array images produced by the 
        fitting stage of the LandTrendr algorithm: B1, B2, B3, B4, B5, B7
        The band used to segment the time-series does not matter.
        
        Parameters:
            ltr_output (ee.Image): An ee.Image of the LandTrendr output with the fitted values for the 
                                   bands: B1, B2, B3, B4, B5, B7
            
        Returns:
            ee.ImageCollection A collection of the LandTrendr interpolated medoid images
        
        '''
        #Cast the years as ee.Numbers to get the index values
        ee_start_year = ee.Number(self.__start_year)
        ee_end_year = ee.Number(self.__end_year)
        
        #Compute the start and end indices to slice the LandTrendr outputs
        start_index = 0
        end_index = ee_end_year.subtract(ee_start_year)
        
        #Select the fitted bands of the LandTrendr output
        fitted_values = ltr_output.select(['B1_fit','B2_fit','B3_fit','B4_fit','B5_fit','B7_fit']).toArray(1)
        
        #Define an ee.List of the years
        years = ee.List.sequence(start_index, end_index)
        
        #Flatten a single from the LandTrendr fitted values
        def flatten_single_year (index):
        
            #Cast the input year as an ee.Number and define the current date
            index = ee.Number(index)
            date = ee.Date.fromYMD(index.add(ee_start_year), 7, 1).millis()
            
            #Define the layer used to unmask the fitted values
            unmask_layer = ee.Image([0,0,0,0,0,0]).toArray().toArray(1).arrayTranspose()
            
            #Slice out the values of the given year
            year_fit = fitted_values.unmask(unmask_layer) \
                .arraySlice(0, index.toInt16(), index.add(1).toInt16()) \
                .arrayProject([1])
            
            #Flatten the fitted spectal values for a single year
            return year_fit.arrayFlatten([['B1','B2','B3','B4','B5','B7']]) \
                .unmask(ee.Image([0,0,0,0,0,0])) \
                .set('system:time_start', date) \
                .toFloat()
            
        
        #Map the flattening function of the list of years to get a 
        #Time-series of interpolated images
        outputs = ee.ImageCollection.fromImages(years.map(flatten_single_year))
        
        return outputs

    def __export_fitted_composites(self, time_series, export_sub_dir):
        '''
        Exports a time-series of medoid_composites to a GEE asset collection whose's path
        is defined by the feature_name and additional_path information passed to the object
        when it is instantiated. 
        
        The export tasks are stored to a dictionary (a class attribute) so that the status of the exports can 
        be queried later. 
        
        Parameters:
            time-series (ee.ImageCollection): A collection of interpolated Landsat medoid composites.
            export_sub_dir (string): The name of the sub-directory (an asset ImageCollection) within the main 
                                     GEE project asset directory. 
            
        Returns:
            None
        
        '''
        # Create a dictionary to store the export_tasks
        export_tasks = {}
           
        # Create the base path for the subsequent exports
        export_path_base = 'users/'+ self.__username+'/'+self.__project_name+'/'+export_sub_dir+'_2/'
     
        # Loop through each year of the time-series
        for year in range(self.__start_export_year, self.__end_export_year+1):
            
            # Define the export asset name
            export_asset_name = export_sub_dir + '_' + str(int(year))
            
            # Construct the date objects used to filter the time-series
            start_date = ee.Date.fromYMD(year, 1, 1)
            end_date = ee.Date.fromYMD(year, 12, 31)
            
            # Set the image's metdata date
            if export_sub_dir == 'spring_nbr':
                image_date = ee.Date.fromYMD(year, 3, 1).millis()
            elif export_sub_dir == 'summer_nbr' or export_sub_dir == 'summer_b5':
                image_date = ee.Date.fromYMD(year, 6, 1).millis()
            elif export_sub_dir == 'fall_nbr':
                image_date = ee.Date.fromYMD(year, 9, 1).millis()
            
            # Select out the current image to be processed
            export_image = ee.Image(time_series.filterDate(start_date, end_date).first()) \
                .set('system:time_start', image_date)
            
            # Execute the export of the current year's image to an asset collection
            task = ee.batch.Export.image.toAsset(
                image = export_image.toInt16(), 
                description = 'Export-Image-' + export_asset_name,
                assetId = export_path_base + export_asset_name,
                maxPixels = 1e13, 
                region = self.__study_area_geo,
                crs = 'EPSG:5070', 
                crsTransform = '[30.0, 0, 15.0, 0, -30.0, 15.0]' 
                )
            task.start()
            
            # Append the information to the export_tasks dictionary
            export_tasks[export_asset_name] = task
            
        # Store the export tasks as a data attribute of the class
        self.__covariate_export_tasks = {**self.__covariate_export_tasks, **export_tasks}
        
        return None
    
if __name__ == "__main__":
    
    # Define years that will be included in the time-series
    start_year = 1986
    end_year = 2020
    
    # Define the years that will be exported as assets
    start_export_year = 2011
    end_export_year = 2020

    study_area = ee.Geometry.Polygon([[[-123.31, 44.59],[-123.31, 44.53],[-123.21, 44.53],[-123.21, 44.59]]], None, False)
    
#    study_area = ee.FeatureCollection('TIGER/2018/States').filterMetadata('NAME', 'equals', 'Oregon').geometry()
    project_name = 'bird_species_2020'
    
    # Specify the GEE account name to which the data will be exported
    # Make sure that this account has permissions with the GEE python API
    username = 'JohnBKilbride'
    
    # Get the metrics
    generator = GenerateSeasonalMetrics(start_year, end_year, start_export_year, end_export_year,
                                   study_area, project_name, username)
    generator.generate_metrics()
   
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
