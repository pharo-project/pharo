Extension { #name : #ClySystemSearchDialogContext }

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClySystemSearchDialogContext class >> selectionStrategyForQueryBrowser [
	<classAnnotation>
	^ClyContextSelectionStrategyAnnotation for: (ClyBrowserDialogContext ofSearchIn: ClyQueryBrowserMorph)
]
