mimic various enumerating
select: aBlock as: aCollectionClass
	| collection |
	collection := self newAs: aCollectionClass.
	self do: [ :each | 
		(aBlock value: each) 
			ifTrue: [ collection add: each ] ].
	^ collection