accessing
contents
	^ self isBinary
		ifTrue:  [ self binaryContents ]
		ifFalse: [ self rawBuffer readStringUTF8 ]
	
	