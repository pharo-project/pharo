tests
testChildren
	| tree |
	tree := GLMTreePresentation new.
	tree transformation: [:aNumber | (1 to: aNumber ) collect: #printString ].
	tree children: [:each | self assert: (each isKindOf: String)  ].
	tree entity: 42.