Extension { #name : 'OCMessageNode' }

{ #category : '*Deprecated14-Kernel' }
OCMessageNode >> argumentPartStrings [

	self deprecated: 'Use argumentNames instead' transformWith: '`@rcv argumentPartStrings' -> '`@rcv argumentNames'.
	^ self argumentNames
]
