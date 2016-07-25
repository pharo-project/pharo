mimic various enumerating
reject: aBlock thenCollect: aBlock2 as: aCollectionClass
	| collection |
	collection := self newAs: aCollectionClass.
	self reject: aBlock thenDo: [ :each | 
		collection add: (aBlock2 value: each) ].
	^ collection