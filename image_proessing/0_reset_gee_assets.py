import ee
from os import system

ee.Initialize()

# Specify the username and the project name (should match what you did in script 1)
username = 'JohnBKilbride'
project_name = 'bird_species_2020'

# Define the image collections
spring = ee.ImageCollection('users/'+username +'/'+project_name+'/spring_nbr')
summer_nbr = ee.ImageCollection('users/'+username +'/'+project_name+'/summer_nbr')
summer_b5 = ee.ImageCollection('users/'+username +'/'+project_name+'/summer_b5')
fall = ee.ImageCollection('users/'+username +'/'+project_name+'/fall_nbr')

# Create a list of all images that need to be removed from the User's GEE assets
spring_images = spring.aggregate_array('system:id').getInfo()
summer_nbr_images = summer_nbr.aggregate_array('system:id').getInfo()
summer_b5_images = summer_b5.aggregate_array('system:id').getInfo()
fall_images = fall.aggregate_array('system:id').getInfo()
remove_images = spring_images + summer_nbr_images + summer_b5_images + fall_images

# Remove the images
for i, image in enumerate(remove_images):
    print(str(i) + '. Removing: ' + image)
    system('earthengine rm ' + image)




