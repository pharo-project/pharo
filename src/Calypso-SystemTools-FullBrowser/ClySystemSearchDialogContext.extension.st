Extension { #name : #ClySystemSearchDialogContext }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClySystemSearchDialogContext class >> selectionStrategyForFullBrowser [
	<classAnnotation>
	^ClyContextSelectionStrategyAnnotation for: (ClyBrowserDialogContext ofSearchIn: ClyFullBrowserMorph)
]
