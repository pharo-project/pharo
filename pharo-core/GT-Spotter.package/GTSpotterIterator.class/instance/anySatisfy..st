mimic various enumerating
anySatisfy: aBlock
	self do: [ :each | (aBlock value: each) ifTrue: [ ^ true ] ].
	^ false