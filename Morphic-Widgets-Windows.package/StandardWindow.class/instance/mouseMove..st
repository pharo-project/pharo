morphic ui
mouseMove: evt
	"Handle a mouse-move event"

	self isFullscreen ifTrue: [^self]. "do nothing if fullscreen"
	^super mouseMove: evt