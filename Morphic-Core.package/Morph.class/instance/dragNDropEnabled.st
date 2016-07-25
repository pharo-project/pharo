dropping/grabbing
dragNDropEnabled
	"Note: This method is only useful for dragEnabled == dropEnabled at all times"
	self separateDragAndDrop.
	^self dragEnabled and:[self dropEnabled]