mimic various enumerating
collect: aBlock as: aCollectionClass
	| collection |
	collection := self newAs: aCollectionClass.
	self do: [ :each | 
		collection add: (aBlock value: each) ].
	^ collection