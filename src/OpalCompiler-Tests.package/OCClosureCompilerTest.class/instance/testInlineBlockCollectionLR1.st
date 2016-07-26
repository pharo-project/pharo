tests
testInlineBlockCollectionLR1
	"Test case from Lukas Renggli"
	| col |
	col := OrderedCollection new.
	1 to: 11 do: [ :each | col add: [ each ] ].
	self assert: (col collect: [ :each | each value ]) asArray = (1 to: 11) asArray