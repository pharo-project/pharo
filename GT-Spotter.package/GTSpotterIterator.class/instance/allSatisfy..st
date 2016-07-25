mimic various enumerating
allSatisfy: aBlock
	self do: [ :each | (aBlock value: each) ifFalse: [ ^ false ] ].
	^ true