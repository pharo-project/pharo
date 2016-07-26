method changes
removeSelectorChanges: selector class: class 
	"Remove all memory of changes associated with the argument, selector, in 
	this class."

	| chgRecord |
	(chgRecord := changeRecords at: class name ifAbsent: [^ self])
		removeSelector: selector.
	chgRecord hasNoChanges ifTrue: [changeRecords removeKey: class name]