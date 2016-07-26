accessing
relativeRectangle
	"Answer a Rectangle whose top left corner is the receiver's offset position 
	and whose width and height are the same as the receiver."

	^Rectangle origin: self offset extent: self extent