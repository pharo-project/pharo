public access
nextPutImage: aForm
	"Encode the given Form on my stream with default quality."

	^ self nextPutImage: aForm quality: -1 progressiveJPEG: false
