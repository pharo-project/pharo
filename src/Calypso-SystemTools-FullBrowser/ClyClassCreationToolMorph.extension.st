Extension { #name : 'ClyClassCreationToolMorph' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyClassCreationToolMorph class >> fullBrowserClassGroupActivation [
	<classAnnotation>

	^ClyTabActivationStrategyAnnotation for: ClyFullBrowserClassGroupContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyClassCreationToolMorph class >> fullBrowserPackageActivation [
	<classAnnotation>

	^ClyTabActivationStrategyAnnotation for: ClyFullBrowserPackageContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyClassCreationToolMorph class >> fullBrowserProjectActivation [
	<classAnnotation>

	^ClyTabActivationStrategyAnnotation for: ClyFullBrowserProjectContext
]
