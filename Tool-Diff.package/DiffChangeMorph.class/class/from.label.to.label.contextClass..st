instance creation
from: old label: oldLabel to: new label: newLabel contextClass: aClass
	"Answer a new instance of the receiver with the given
	old and new text and descriptions."

	^self new
		from: old
		to: new
		contextClass: aClass;
		fromDescription: oldLabel;
		toDescription: newLabel