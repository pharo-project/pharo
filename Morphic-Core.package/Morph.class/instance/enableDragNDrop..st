dropping/grabbing
enableDragNDrop: aBoolean
	"Set both properties at once"
	self separateDragAndDrop.
	self enableDrag: aBoolean.
	self enableDrop: aBoolean.