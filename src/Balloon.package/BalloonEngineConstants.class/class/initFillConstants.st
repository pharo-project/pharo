pool definition
initFillConstants
	"Initialize the fill constants"

	"Fill primitive types"
	GEPrimitiveFill := 16r100.
	GEPrimitiveLinearGradientFill := 16r200.
	GEPrimitiveRadialGradientFill := 16r300.
	GEPrimitiveClippedBitmapFill := 16r400.
	GEPrimitiveRepeatedBitmapFill := 16r500.

	"General fill state constants"
	GEBaseFillSize := 4.				"Basic size of each fill"

	"Oriented fill constants"
	GFOriginX := 4.				"X origin of fill"
	GFOriginY := 5.				"Y origin of fill"
	GFDirectionX := 6.				"X direction of fill"
	GFDirectionY := 7.				"Y direction of fill"
	GFNormalX := 8.				"X normal of fill"
	GFNormalY := 9.				"Y normal of fill"

	"Gradient fill constants"
	GFRampLength := 10.			"Length of following color ramp"
	GFRampOffset := 12.			"Offset of first ramp entry"
	GGBaseSize := 12.

	"Bitmap fill constants"
	GBBitmapWidth := 10.			"Width of bitmap"
	GBBitmapHeight := 11.			"Height of bitmap"
	GBBitmapDepth := 12.			"Depth of bitmap"
	GBBitmapSize := 13.			"Size of bitmap words"
	GBBitmapRaster := 14.			"Size of raster line"
	GBColormapSize := 15.			"Size of colormap, if any"
	GBTileFlag := 16.				"True if the bitmap is tiled"
	GBColormapOffset := 18.		"Offset of colormap, if any"
	GBMBaseSize := 18.			"Basic size of bitmap fill"
