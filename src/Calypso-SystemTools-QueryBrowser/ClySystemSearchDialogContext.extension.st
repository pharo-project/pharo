Extension { #name : #ClySystemSearchDialogContext }

{ #category : #'*Calypso-SystemTools-QueryBrowser' }
ClySystemSearchDialogContext class >> selectionStrategyForQueryBrowser [
	<classAnnotation>
	^ClyContextSelectionStrategy for: (ClyBrowserDialogContext ofSearchIn: ClyQueryBrowser)
]
