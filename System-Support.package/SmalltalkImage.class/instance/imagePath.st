image, changes name
imagePath
	"Answer the full path name for the current image."

	"SmalltalkImage current imagePath"

	^ FilePathEncoder decode: self primImagePath