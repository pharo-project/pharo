event handling
mouseDown: evt
	"If the shift key is pressed, make this string the keyboard input focus.
	Process as normal if StringMorph class>> editableStringMorph  returns false."

	(self isEditable: evt)
		ifTrue: [self launchEditor: evt]
		ifFalse: [super mouseDown: evt].
