accessing
subexpression: subIndex
	"Answer a string that matched the subexpression at the given index.
	If there are multiple matches, answer the last one.
	If there are no matches, answer nil. 
	(NB: it used to answer an empty string but I think nil makes more sense)."

	| matches |
	matches := self subexpressions: subIndex.
	^matches isEmpty ifTrue: [nil] ifFalse: [matches last]