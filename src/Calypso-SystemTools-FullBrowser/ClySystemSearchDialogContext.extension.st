Extension { #name : #ClySystemSearchDialogContext }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClySystemSearchDialogContext class >> selectionStrategyForFullBrowser [
	<classAnnotation>
	^ClyContextSelectionStrategy for: (ClyBrowserDialogContext ofSearchIn: ClyFullBrowser)
]
