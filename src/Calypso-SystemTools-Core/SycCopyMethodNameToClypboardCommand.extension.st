Extension { #name : 'SycCopyMethodNameToClypboardCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycCopyMethodNameToClypboardCommand class >> methodTabIconActivation [
	<classAnnotation>

	^ ClyBrowserTabCommandActivation for: ClyMethod asCalypsoItemContext
]
