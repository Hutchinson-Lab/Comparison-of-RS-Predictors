import ee
from datetime import datetime

ee.Initialize()

def main (sample_points, start_year, end_year, export_prefix):
    '''
    Main logic --> runs the sampling proceedure
    
    Primary steps:
        1. Buffer the input point to obtain multiple buffers arond each point
        2. Loading in LandTrendr-derived fitted values. 
        3. Sampling the LandTrendr-derived fitted values with the buffer
        4. Exporting the summarization results as a CSV
        
    Parameters:
        sample_points (ee.FeatureCollection): A FeatureCollection of points which will be used to do the summarization
        start_year (int): The start year of the sampling process
        end_year (int): The end year of the sampling process 
        export_prefix(string): The name of the CSV to export (will have a time stamp appended to this)
        
    Returns:
        None
    '''     
    
    # Get the buffered sample points
    buff_points = get_buffered_points(sample_points)
      
    # Load in the Landsat time-series metric
    landsat_metrics = load_image_data(start_year, end_year)
      
    # Run the sampling proceedure
    sampled_values = run_sampling(buff_points, landsat_metrics)
    
    # Export the ee.FeatureCollection to Google Drive
    time_stamp = get_time_stamp()
    task = ee.batch.Export.table.toDrive(
        collection = sampled_values, 
        description = 'Export-Table-Bird-Habitat-Buffers', 
        fileNamePrefix = export_prefix + time_stamp,
        fileFormat = 'CSV'
        )
    task.start()
     
    return None

def add_buffer (points, buffer_distance):
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
    # Do the inner map to add the geometry
    def add_new_geometry (feat):
        
        # Cast the feature
        feat = ee.Feature(feat)
            
        # Buffer the geometry by the input buffer distance
        buffered_geo = feat.geometry().buffer(buffer_distance, 0.1)
            
        return feat.setGeometry(buffered_geo).set('buffer_distance_m', buffer_distance)
        
    return points.map(add_new_geometry)

def get_buffered_points (input_points):
    '''
    Construct a series of buffers around the input points. Distances: 75, 150, 300, 600, 1200, and 2400.
    These are then combined into a single image collection
    
    Parameters:
        input_points (ee.FeatureCollection): An input feature collection of points to buffer
        
    Returns
        An ee.FeatureCollection which contains all of the buffers, for each point, as individual features.
    '''
    points_75 = add_buffer(input_points, 75)
    points_600 = add_buffer(input_points, 600)
    points_2400 = add_buffer(input_points, 2400)
        
    return points_75.merge(points_600).merge(points_2400)

def load_image_data (start_year, end_year):
    '''

    
    Parameters:
        start_year (int): 
        end_year (int): 
    
    Returns:
        ee.ImageCollection The time-series of landsat imagery
    
    '''   
  
    # Load in the four different landsat collections
    summer_nbr = ee.ImageCollection('users/JohnBKilbride/bird_species_2020/summer_nbr')
    
    # Compute the metrics required for each of the time-series
    summer_nbr = compute_spectral_metrics(summer_nbr, 'summer_nbr')

    # Combine the image collections into a single ee.ImageCollectiion with one image for each yearof the time-series
    all_metrics = compute_dem_metrics(summer_nbr)
      
    return all_metrics
  
def compute_dem_metrics (input_collection):
    '''
    Parameters:
    start_year (int): 
    end_year (int): 
    
    Returns:
    ee.ImageCollection The time-series of landsat imagery
    
    '''  
    # Compute the the slope, elevation, and aspect from the National Elevation Dataset
    ned_dem = ee.Algorithms.Terrain(ee.Image('USGS/NED')).select(['elevation', 'slope', 'aspect'])
      
    # Add the elevation band to an image
    def inner_map (image):
        return image.addBands(ned_dem).toFloat()
     
    return input_collection.map(inner_map)

def scale_landsat_bands (image):
    '''
    Scale the Landsat bands by multiplying the SR values by 0.0001. 
    Output will be converted to floats.
    
    Parameters:
        image (ee.Image): An image whose values will be scaled by the constant
    
    Returns
        ee.Image with floating point values
    
    '''
    return image.multiply(0.0001).set('system:time_start', image.date().millis())

def compute_tasseled_components (image):
    '''
    Computes the Tasseled Cap Coefficents using the 
    
    Parameters
        
    
    Returns
    
    '''
    # Calculate Tasseled Cap Brightness, Greenness, Wetness
    bands = image.select(['B1','B2','B3','B4','B5','B7'])
    coefficients = ee.Array([ 
        [0.2043, 0.4158, 0.5524, 0.5741, 0.3124, 0.2303],
        [-0.1603, -0.2819, -0.4934, 0.7940, -0.0002, -0.1446],
        [0.0315, 0.2021, 0.3102, 0.1594, -0.6806, -0.6109],
        ])
    componentsImage = ee.Image(coefficients) \
        .matrixMultiply(bands.toArray().toArray(1)) \
        .arrayProject([0]) \
        .arrayFlatten([['TCB','TCG','TCW']]) \
        .toFloat()
    image = image.addBands(componentsImage)
    
    # Add Tasseled Cap Angle 
    tcb = image.select(['TCB'])
    tcg = image.select(['TCG'])
    image = image.addBands(tcg.divide(tcb).atan().rename(['TCA']))
    
    return image
 
def compute_ndvi (image):
    '''
    Computes the Normalized Difference Vegitation Index.
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with NDVI added as a new band.
    '''
    return image.addBands(image.normalizedDifference(['B4', 'B3']).rename('NDVI')).toFloat()

def compute_nbr (image):
    '''
    Computes the Normalized Burn Ratio.
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the index added as a new band.
    '''
    return image.addBands(image.normalizedDifference(['B4', 'B7']).rename('NBR')).toFloat()

def compute_nbr_2 (image):
    '''
    Computes the Normalized Burn Ratio 2.
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the index added as a new band.
    '''
    return image.addBands(image.normalizedDifference(['B5', 'B7']).rename('NBR_2')).toFloat()

def compute_ndmi (image):
    '''
    Computes the Normalized Difference Moisture Index.
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the index added as a new band.
    '''
    return image.addBands(image.normalizedDifference(['B5','B4']).rename(['NDMI'])).toFloat()

def compute_evi (image):
    '''
    Computes the Enhanced Vegitation Index
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the index added as a new band.
    '''
    # Select out the bands we need
    b1 = image.select('B1')
    b3 = image.select('B3')
    b4 = image.select('B4')
    
    # Compute the different terms
    numerator = b4.subtract(b3)
    denominator = b4.add(6).multiply(b3).subtract(b1.multiply(7.5)).add(1)
    
    # Compute the EVI
    evi = numerator.divide(denominator).multiply(2.5)
    
    return image.addBands(evi.rename(['EVI'])).toFloat()

def compute_savi (image):
    '''
    Computes the Soil Adjusted Vegitation Index.
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the index added as a new band.
    '''    
    # Select out the bands we need
    b3 = image.select('B3')
    b4 = image.select('B4')
    
    # Compute the different terms
    numerator = b4.subtract(b3)
    denominator = b4.add(b3).add(0.5)
    
    # Compute the EVI
    savi = numerator.divide(denominator).multiply(1.5)
    
    return image.addBands(savi.rename(['SAVI'])).toFloat()

def compute_ndsi (image):
    '''
    Computes the Normalized Difference Snow Index.
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the index added as a new band.
    '''
    return image.addBands(image.normalizedDifference(['B2', 'B5']).rename('NDSI')).toFloat()

def compute_msavi (image):
    '''
    Computes the Modified Soil Adjusted Vegitation Index
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the index added as a new band.
    '''
    # Select out the bands we need
    b3 = image.select('B3')
    b4 = image.select('B4')
    
    # Compute the different terms
    term1 = b4.multiply(2)
    term2 = b4.multiply(2).add(1).pow(2)
    term3 = b4.subtract(b3).multiply(8)
    
    # Compute the EVI
    msavi = term1.add(1).subtract(term2.subtract(term3).sqrt()).divide(2)
    
    return image.addBands(msavi.rename(['MSAVI'])).toFloat()

def compute_swir2_glcm (image):
    '''
    Computes several Gray Level Co-occurance Matrix statistics using the SWIR 2 band
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the GLCM metrics  added as a new band.
    '''
    # Scale the floats to integers and then compute the metrics with a 4x4 kernel
    glcm = image.select('B7').multiply(10000).toInt16().glcmTexture(size=4)

    # GLCM Metric names
    original_names = ["B7_contrast","B7_corr","B7_var","B7_inertia","B7_shade",
                      "B7_prom","B7_idm","B7_ent"]
    new_names = ["B7_Contrast","B7_Correlation","B7_Variance","B7_Inertia","B7_Shade",
                 "B7_Prominence","B7_Energy","B7_Entropy"]
    
    # Select the metrics we want
    glcm_metrics = glcm.select(original_names).rename(new_names)
    
    return image.addBands(glcm_metrics).toFloat()

def compute_nir_glcm (image):
    '''
    Computes several Gray Level Co-occurance Matrix statistics using the NIR band
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the GLCM metrics  added as a new band.
    '''
    # Scale the floats to integers and then compute the metrics with a 4x4 kernel
    glcm = image.select('B4').multiply(10000).toInt16().glcmTexture(size=4)
    
    # GLCM Metric names
    original_names = ["B4_contrast","B4_corr","B4_var","B4_inertia","B4_shade",
                      "B4_prom","B4_idm","B4_ent"]
    new_names = ["B4_Contrast","B4_Correlation","B4_Variance","B4_Inertia","B4_Shade",
                 "B4_Prominence","B4_Energy","B4_Entropy"]
    
    # Select the metrics we want
    glcm_metrics = glcm.select(original_names).rename(new_names)
    
    return image.addBands(glcm_metrics).toFloat()

def compute_spectral_metrics (input_collection, prefix):
    '''
    Computes the following spectral metrics using Landsat data:
        Tasseled Cap Components -- Brightness, Greenness, Wetness
        Normalized Difference Vegitation Index
        Normalized Burn Ratio
        Normalized Burn Ratio 2
        Normalized Difference Mosuiture Index
        Enhanced Vegitation Index
        Soil Adjusted Vegitation Index
        Normalized Difference Snow Index
        Modified Soil Adjusted Vegitation INdex
        Gray Level Co-occurance Matrix - Computed using SWIR 2
        Gray Level Co-occurance Matrix - Computed using NIR
    
    Parameters:
        image (ee.ImageCollection): The image is assumed to be a Landsat image with TM/ETM+ band names.
        prefix (string): A string containing a prefix to append to the band names
        
    Returns:
        The input image with spectral metrics 
    '''
    # Compute the spectral indicies
    input_collection = input_collection.map(scale_landsat_bands) \
        .map(compute_tasseled_components) \
        .map(compute_ndvi) \
        .map(compute_nbr) \
        .map(compute_nbr_2) \
        .map(compute_ndmi) \
        .map(compute_evi) \
        .map(compute_savi) \
        .map(compute_ndsi) \
        .map(compute_msavi) \
        .map(compute_swir2_glcm) \
        .map(compute_nir_glcm)
        
    # Define the final set of band names
    def add_prefix (band_name):

        # Cast the band name as an ee.String
        band_name = ee.String(band_name)
              
        # Append the prefix to the band name (note 'prefix' is define in a higher scope)
        return ee.String(prefix).cat('_').cat(band_name)
   
    # Rename the bands to include the prefix
    def rename_image_bands (image):
   
        # Get the band names
        final_band_names = image.bandNames().map(add_prefix)
            
        # Rename the bands
        image = image.select(image.bandNames(), final_band_names)
            
        # Add the year band (the name of this band is constant across different collections and doesn't get a prefix)
        image_year = ee.Date(image.get('system:time_start')).get('year').toFloat()
        image = image.addBands(ee.Image.constant(image_year).rename('image_year'))
            
        return image.toFloat()
    
    # Rename the bands
    output = input_collection.map(rename_image_bands)
      
    return output

def run_sampling (sample_locations, imagery):
    '''
    Using the input collections of points and imagery, the function will loop throught the points
    and extract the infromation from the 
    
    Parameters:
        sample_locations (ee.FeatureCollection): Locations to suimmarize the spectral information.
        imagery (ee.IamgeCollection): Imagery whose metrics will be sampled usign the geometry of the sample locations
    
    Returns:
        ee.FeatureCollection

    '''
      
    def inner_map (input_feature):
            
        # Get the year from the feature
        input_year = ee.Number(ee.Feature(input_feature).get('Year'))
            
        # Extract the correct image
        start_date = ee.Date.fromYMD(input_year, 1, 1)
        end_date = ee.Date.fromYMD(input_year, 12, 31)
        sample_layer = ee.Image(imagery.filterDate(start_date, end_date).first())
            
        # Run the sampling
        custom_reducer = ee.Reducer.mean().combine(ee.Reducer.stdDev(), None, True)
        sampled = sample_layer.reduceRegion(
            reducer = custom_reducer, 
            geometry = input_feature.geometry(), 
            crs = 'EPSG:5070', 
            scale = 30,
            maxPixels = 1e10
            )
        
        return input_feature.set(sampled)
        
    return ee.FeatureCollection(sample_locations.map(inner_map))

def generate_names():
    '''
    Computes several Gray Level Co-occurance Matrix statistics using the SWIR 2 band
    
    Parameters:
        image (ee.Image): The image is assumed to be a Landsat image with TM/ETM+ band names.
    
    Returns:
        The input image with the GLCM metrics  added as a new band.
    '''

    # Define these thangs
    seasons = [ 'summer_nbr']
    spectral_metrics = ["B1","B2","B3","B4","B5","B7","TCB","TCG","TCW","TCA","NDVI","NBR",
                        "NBR_2","NDMI","EVI","SAVI","NDSI","MSAVI","B4_Contrast","B4_Correlation",
                        "B4_Variance","B4_Inertia","B4_Shade","B4_Prominence","B4_Energy",
                        "B4_Entropy", "B7_Contrast","B7_Correlation","B7_Variance",
                        "B7_Inertia","B7_Shade","B7_Prominence","B7_Energy","B7_Entropy"
                        ]
    metric_types = ['mean', 'stdDev']

    # Loop over that shit
    all_spec_metrics = []
    for season in seasons:
        for spectral_metric in spectral_metrics:
            for metric_type in metric_types:
                all_spec_metrics.append(season + '_' + spectral_metric + '_' + metric_type)
    for metric in ['slope', 'elevation', 'aspect']:
        for metric_type in metric_types:
            all_spec_metrics.append(metric + '_' + metric_type)

    # Define the other metrics
    other_cols = ['Unique_Checklist_ID', 'buffer_distance_m']

    return other_cols + all_spec_metrics

def get_time_stamp():
    '''
    Generates a time-stamp to append ot the export.    
    
    Parameters:
        None
    
    Returns:
        A python string containing the day, month, year, hour, mintue, and second of the fucntion call
    '''
    
    # datetime object containing current date and time
    now = datetime.now()
     
    # dd/mm/YY H:M:S
    time_stamp = now.strftime("%d_%m_%Y_%H_%M_%S")

    return time_stamp

if __name__ == '__main__':
    
    # Define the start and the end years of the sampling period
    start_sample_year = 2011
    end_sample_year = 2019

    # Load in the EBird data
    oregon_2020_points = ee.FeatureCollection('users/JohnBKilbride/oregon_2020_laurel_years') \
        .select(["Unique_Che","year"], ["Unique_Checklist_ID","Year"]) 
        # .randomColumn().sort('random').limit(10)
    
    main(oregon_2020_points, start_sample_year, end_sample_year, 'laurel_ebird_oregon_2020_')


print('Program complete.')





