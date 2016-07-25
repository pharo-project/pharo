tests
testInlineBlockCollectionLR3
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | | i | i := each. col add: [ i ]. i := i + 1 ].
	self assert: (col collect: [ :each | each value ]) asArray = (2 to: 12) asArray