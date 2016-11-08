running
check: aNode forCritiquesDo: aBlock

	aNode methodNode ifNotNil: [ :methNode |
		(self checkPreconditionOn: methNode) ifTrue: [
			super check: aNode forCritiquesDo: aBlock ] ]