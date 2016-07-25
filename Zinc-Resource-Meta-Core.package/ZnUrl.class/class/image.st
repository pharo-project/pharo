accessing
image
	"Return a File URL refering to the file path of the current image file.
	This is useful as a context to resolve relative URLs, see #withRelativeReference:"
	
	^ SmalltalkImage current imagePath asPath asZnUrl