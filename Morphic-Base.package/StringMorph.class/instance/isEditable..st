testing
isEditable: evt
	"If the shift key is pressed then yes.
	As normal if StringMorph class>> editableStringMorph  returns false."
	
	^(self isEditable and: [evt shiftPressed])
