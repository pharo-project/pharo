mimic various enumerating
reject: aBlock as: aCollectionClass
	| collection |
	collection := self newAs: aCollectionClass.
	self do: [ :each | 
		(aBlock value: each) 
			ifFalse: [ collection add: each ] ].
	^ collection