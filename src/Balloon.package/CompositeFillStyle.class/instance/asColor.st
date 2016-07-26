converting
asColor
	"Answer a colour that is a best match to the receiver.
	Simple approach for the moment."
	
	^self fillStyles
		ifEmpty: [Color transparent]
		ifNotEmpty: [self fillStyles last asColor]