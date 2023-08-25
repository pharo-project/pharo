Extension { #name : #RPackage }

{ #category : #'*Deprecated12' }
RPackage >> isYourClassExtension: protocol [

	self
		deprecated: 'Use #isExtensionMatching: on the parameter instead because the name is much more explicit.'
		transformWith: '`@rcv isYourClassExtension: `@arg' -> '`@arg isExtensionProtocolMatching: `@rcv'.

	^ protocol isExtensionProtocolMatching: self
]
