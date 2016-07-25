mimic various enumerating
select: aBlock thenCollect: aBlock2 as: aCollectionClass
	| collection |
	collection := self newAs: aCollectionClass.
	self select: aBlock thenDo: [ :each | 
		collection add: (aBlock2 value: each) ].
	^ collection