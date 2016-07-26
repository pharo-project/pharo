testing
isImageFile: aString
	"Answer whether the file name indicates an image file."

	aString ifNil: [^false].
	^#('pcx' 'bmp' 'jpeg' 'xbm' 'pnm' 'ppm' 'gif' 'pam' 'jpg' 'png' 'pbm')
		includes: aString asFileReference extension asLowercase