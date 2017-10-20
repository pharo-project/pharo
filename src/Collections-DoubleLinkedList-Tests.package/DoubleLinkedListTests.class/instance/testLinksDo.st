testing
testLinksDo
	| list links index |
	list := DoubleLinkedList new.
	links := OrderedCollection new.
	1 to: 10 do: [ :each |
		links add: (list add: each) ].
	index := 1.
	list linksDo: [ :each |
		self assert: each equals: (links at: index).
		self assert: each value equals: index.
		index := index + 1 ]