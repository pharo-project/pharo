model access
getSelection
	"Answer the model's selection interval.
	If not available keep the current selection."

	getSelectionSelector isNil ifFalse: [^super getSelection].
	^selectionInterval ifNil: [super getSelection]