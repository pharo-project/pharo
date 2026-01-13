Extension { #name : 'SmalltalkImage' }

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> isHeadless [

	self deprecated: 'This method will always answer true now. It should either be replaced by a usage of #isInteractive or be removed'.
	^ true
]

{ #category : '*Deprecated14-Kernel' }
SmalltalkImage >> isInteractiveGraphic [

	self deprecated: 'Use #isInteractive instead.' transformWith: '`@rcv isInteractiveGraphic' -> '`@rcv isInteractive'.
	^ self isInteractive
]
