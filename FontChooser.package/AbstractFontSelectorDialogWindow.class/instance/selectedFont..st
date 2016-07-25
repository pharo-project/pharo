accessing
selectedFont: anObject
	"Set the value of selectedFont"

	selectedFont := anObject ifNil: [TextStyle defaultFont].
	self updateFromSelectedFont