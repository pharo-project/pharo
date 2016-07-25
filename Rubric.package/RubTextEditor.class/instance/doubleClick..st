events
doubleClick: evt
	| here |
	self closeTypeIn.	"no matter what, if shift is pressed extend the selection"
	here := self pointIndex.
	(here between: 2 and: self string size)
		ifFalse: [ 
			"if at beginning or end, select entire string"
			self selectFrom: 1 to: self string size ]
		ifTrue: [ self selectWord ].
	self setEmphasisHereFromText.
	self storeSelectionInText