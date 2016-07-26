as yet unclassified
stopMethod: aMethod during: aBlock

	(activeMethods includes: aMethod) ifTrue: [ ^ self ].
	
	activeMethods add: aMethod.
	
	aBlock ensure: [ activeMethods remove: aMethod ]