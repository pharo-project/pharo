event handling
handlesMouseDown: evt
	"If the shift key is pressed then yes.
	As normal if StringMorph class>> editableStringMorph  returns false."
	
	^ (self isEditable: evt)
		ifTrue: [true]
		ifFalse: [super handlesMouseDown: evt]