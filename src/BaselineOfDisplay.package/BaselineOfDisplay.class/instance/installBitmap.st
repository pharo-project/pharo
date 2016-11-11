actions
installBitmap

	"install new Bitmap class to special objects array"

	| array |
	
	array := Smalltalk specialObjectsArray copy.
	array at: 5 put: Bitmap.
	Smalltalk specialObjectsArray becomeForward: array.