Extension { #name : #RG2Definition }

{ #category : #'*Epicea' }
RG2Definition >> asEpiceaRingDefinition [

	| aDefinition |
	
	aDefinition := self.
	aDefinition environment backend: (RG2ReadOnlyImageBackend for: aDefinition environment).
	aDefinition resolveBehaviorInformation.
	aDefinition environment backend: RG2EnvironmentBackend new. 
	aDefinition environment cleanAllProperties.
	^ aDefinition. 
]
