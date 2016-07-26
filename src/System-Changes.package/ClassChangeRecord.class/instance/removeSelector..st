method changes
removeSelector: selector
	"Remove all memory of changes associated with the argument, selector, in this class."

	selector == #Comment
		ifTrue:
			[changeTypes remove: #comment ifAbsent: []]
		ifFalse:
			[methodChanges removeKey: selector ifAbsent: []]