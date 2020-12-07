Extension { #name : #ClyClassCreationToolMorph }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyClassCreationToolMorph class >> fullBrowserClassGroupActivation [
	<classAnnotation>
	
	^ClyTabActivationStrategy for: ClyFullBrowserClassGroupContext
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyClassCreationToolMorph class >> fullBrowserPackageActivation [
	<classAnnotation>
	
	^ClyTabActivationStrategy for: ClyFullBrowserPackageContext
]
