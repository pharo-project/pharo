images
arrowOfDirection: aSymbol size: finalSizeInteger color: aColor 
	"answer a form with an arrow based on the parameters"
	^ ArrowImagesCache at: {aSymbol. finalSizeInteger. aColor}