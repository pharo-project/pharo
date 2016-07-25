private
buildParseTree
	| tree |
	tree := self selectedClass parseTreeFor: self selector.
	tree isNil ifTrue: [^RBParser parseMethod: 'method'].
	^tree