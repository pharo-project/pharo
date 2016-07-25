private
setDismissColor: evt with: dismissHandle
	"Called on mouseStillDown in the dismiss handle; set the color appropriately."

	| colorToUse |
	evt hand obtainHalo: self.
	colorToUse :=  (dismissHandle containsPoint: evt cursorPoint)
		ifFalse:
			[Color red muchLighter]
		ifTrue:
			[Color lightGray].
	self setColor: colorToUse toHandle: dismissHandle.
