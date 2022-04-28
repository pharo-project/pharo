Extension { #name : #AbstractProtocol }

{ #category : #'*Ring-Core' }
AbstractProtocol >> asRingMinimalDefinitionIn: anRGEnvironment [

	self error: 'For Protocol you need to use #asRingMinimalDefinitionIn:parent: because parent class cannot be resolved automatically'
]
