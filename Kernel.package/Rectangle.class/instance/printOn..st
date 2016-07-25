printing
printOn: aStream 
	"Refer to the comment in Object|printOn:."

	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream