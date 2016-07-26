event handling
keyStroke: evt
	"Abort the drag on an escape"
	evt keyCharacter ~= Character escape ifTrue: [ ^self ].
	self delete.