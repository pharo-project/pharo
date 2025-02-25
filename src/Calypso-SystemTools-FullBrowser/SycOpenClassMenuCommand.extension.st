Extension { #name : 'SycOpenClassMenuCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycOpenClassMenuCommand class >> fullBrowserTabActivation [
	<classAnnotation>

	^ClyBrowserTabCommandActivation for: ClyFullBrowserClassContext
]
