from osgeo import gdal, ogr
import sys
import os
# this allows GDAL to throw Python Exceptions
gdal.UseExceptions()

#
#  get raster datasource
#


# Change directory
currentDir = os.getcwd()
dirName = 'tempimages'
os.chdir(os.getcwd() + '/' + dirName)
tifDir = os.getcwd()

tifFiles = [f for f in os.listdir(tifDir) if f.endswith('.tif')]
for i in range(len(tifFiles)):
	tifFiles[i] = tifFiles[i][:-4]

#create output directory
outputDir = currentDir + "/tempshapes/"
if not os.path.exists(outputDir):
		os.makedirs(outputDir)

for i in range(len(tifFiles)): 
	imageName = tifFiles[i]
	os.chdir(tifDir)
	src_ds = gdal.Open( imageName + '.tif')
	if src_ds is None:
		print 'Unable to open %s' % src_filename
		sys.exit(1)

	try:
		srcband = src_ds.GetRasterBand(1)
	except RuntimeError, e:
		# for example, try GetRasterBand(10)
		print 'Band ( %i ) not found' % band_num
		print e
		sys.exit(1)

	os.chdir(outputDir)

	#
	#  create output datasource
	#
	dst_layername = imageName
	drv = ogr.GetDriverByName("ESRI Shapefile")
	dst_ds = drv.CreateDataSource( dst_layername + ".shp" )
	dst_layer = dst_ds.CreateLayer(dst_layername, srs = None )

	newField = ogr.FieldDefn('MYFLD', ogr.OFTInteger)
	dst_layer.CreateField(newField)

	gdal.Polygonize( srcband, srcband, dst_layer, -1, [], callback=None )
	
	print("Shapefile " + imageName + ".shp done.")

	dst_ds.Destroy()
	src_ds = None


