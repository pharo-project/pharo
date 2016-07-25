accessing
selectedMessageName
	"Answer the method selector or nil if no method change
	is selected.."

	^self selectionIsMethodChange
		ifTrue: [self selectedChange definition selector]
		