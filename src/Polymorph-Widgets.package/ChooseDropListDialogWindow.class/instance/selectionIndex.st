accessing
selectionIndex
	"Answer the initial selection index for the list."

	^self list ifEmpty: [0] ifNotEmpty: [1]