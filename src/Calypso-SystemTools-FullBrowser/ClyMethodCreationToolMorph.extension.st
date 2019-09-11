Extension { #name : #ClyMethodCreationToolMorph }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyMethodCreationToolMorph class >> fullBrowserActivation [
	<classAnnotation>
	
	^ClyTabActivationStrategy for: ClyMethodGroupContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyMethodCreationToolMorph class >> fullBrowserActivation2 [
	<classAnnotation>
	
	^ClyTabActivationStrategy for: ClyVariableContextOfFullBrowser 
]
