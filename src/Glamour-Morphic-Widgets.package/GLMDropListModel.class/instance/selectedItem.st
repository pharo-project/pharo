accessing
selectedItem
	"Answer the currently selected item or nil if none."

	^self selectionIndex = 0
		ifTrue: [nil]
		ifFalse: [self glamourPresentation displayValue at: self selectionIndex]