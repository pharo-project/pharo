image, changes name
imageDirectory
	"Answer the directory containing the current image."

	^ (Path from: self primImagePath) parent