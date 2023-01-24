# script to download the geospatial covariates from google earth engine
# reproducible setup with venv and requirements.txt
# note: the raster files are saved in the corresponding Google Account and have to be moved manually
#

import ee
import datetime

from country_bounding_boxes import country_subunits_by_iso_code


# authentication
#ee.Authenticate()
ee.Initialize()


# country and data set selection
countries = {
    # Country           BBOX                                                                    End           Start
    'Madagascar':       ([43.2571289063, -25.5705078125, 50.4827148438, -12.0795898437],    '2021-07-01', '2020-07-01'),
    'Mali':             ([-12.2806152344, 10.1432617187, 4.23466796875, 24.9956054688],     '2021-08-31', '2020-08-31'),
}

catalog = {
    'evi':                      ('MODIS/061/MOD13A2', 'EVI'),           
    'ndvi':                     ('MODIS/061/MOD13A2', 'NDVI'),           
    'lst_day':                  ('MODIS/061/MOD11A2', 'LST_Day_1km'),   
    'lst_night':                ('MODIS/061/MOD11A2', 'LST_Night_1km'),   
    'precip':                   ('UCSB-CHG/CHIRPS/PENTAD', 'precipitation'),
    'water_mask':               ('MODIS/006/MOD44W', 'water_mask'),
}

features = list(catalog.keys())


# define functions
def kelvin_to_celsius(img):
    return (img.multiply(0.02).subtract(273.15).copyProperties(img, ["system:time_start"]))

def get_raster(BBOX, FILENAME=None, start_date=None, end_date=None, PRODUCT=None, BAND=None):
    
    region = ee.Geometry.Rectangle(BBOX)            # select region
    folder = "Earth Engine"                         # set export folder

    # obtain the image
    global composite
    if (BAND == 'LST_Day_1km' or BAND == 'LST_Night_1km'): 
        composite = (ee.ImageCollection(PRODUCT)
            .filterDate(start_date, end_date)
            .filterBounds(region)
            .select(BAND)
            .map(kelvin_to_celsius)
            .mean())
    elif (BAND == 'water_mask'):
        composite = (ee.ImageCollection(PRODUCT)
            .select(BAND)
            .filterBounds(region)
            .mode())
    elif (BAND == 'EVI' or BAND == 'NDVI'):
        composite = (ee.ImageCollection(PRODUCT)
            .filterDate(start_date, end_date)
            .filterBounds(region)
            .select(BAND)
            .mean()
            .multiply(0.0001))
    elif (BAND == 'precipitation'):
        composite = (ee.ImageCollection(PRODUCT)
            .filterDate(start_date, end_date)
            .filterBounds(region)
            .select(BAND)
            .sum())
    else: 
        sys.exit('Error')


    # export task
    task = ee.batch.Export.image.toDrive(
        image=composite,
        description=FILENAME,
        folder=folder,
        maxPixels=1e13,
        scale=1000,
        region=region,
        crs='EPSG:4326'
    )

    task.start()



# execute task
for country in countries: 

    print(f'Starting for {country}')
    BBOX, end_date, start_date = countries[country]

    for feature in features:
        product, band = catalog[feature]
        fname = f'{country}_{end_date[0:4]}_ee_{feature}'
        print(f'Downloading {feature} with product ID {product} to {fname}')
        get_raster(BBOX, FILENAME = fname, start_date=start_date, end_date=end_date, PRODUCT = product, BAND = band)
