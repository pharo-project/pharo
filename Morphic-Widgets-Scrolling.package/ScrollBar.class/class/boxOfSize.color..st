images
boxOfSize: finalSizeInteger color: aColor 
	"answer a form with an box based on the parameters"
	^ BoxesImagesCache at: {finalSizeInteger. aColor}