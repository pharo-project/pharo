Extension { #name : #RG2Definition }

{ #category : #'*Ring2-Compatibility' }
RG2Definition >> asStandalone [

	| aDefinition |
	
	aDefinition := self copy makeResolved.
	aDefinition environment backend: RG2EnvironmentBackend new. 
	^ aDefinition. 
]
