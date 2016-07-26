mimic various enumerating
detect: aBlock ifFound: aBlock2 ifNone: aBlock3
	self do: [ :each | aBlock value ifTrue: [ ^ aBlock2 value: each ] ].
	^ aBlock3 value