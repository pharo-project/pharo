Extension { #name : #ClyMethodCreationToolMorph }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyMethodCreationToolMorph class >> fullBrowserActivation [
	<classAnnotation>
	
	^ClyTabActivationStrategyAnnotation for: ClyFullBrowserMethodGroupContext 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyMethodCreationToolMorph class >> fullBrowserActivation2 [
	<classAnnotation>
	
	^ClyTabActivationStrategyAnnotation for: ClyFullBrowserVariableContext 
]
