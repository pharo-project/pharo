private
stopMethod: aMethod during: aBlock

	| activeMethods |
	activeMethods := self activeMethods.
	(activeMethods includes: aMethod) ifTrue: [ ^ self ].
	
	activeMethods add: aMethod.
	
	aBlock ensure: [ activeMethods remove: aMethod ]