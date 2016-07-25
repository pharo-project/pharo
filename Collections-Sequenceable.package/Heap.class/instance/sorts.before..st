testing
sorts: element1 before: element2
	"Return true if element1 should be sorted before element2.
	This method defines the sort order in the receiver"
	^sortBlock == nil
		ifTrue:[element1 <= element2]
		ifFalse:[sortBlock value: element1 value: element2].