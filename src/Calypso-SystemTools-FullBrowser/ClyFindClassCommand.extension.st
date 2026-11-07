Extension { #name : 'ClyFindClassCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
ClyFindClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $f meta for: ClyFullBrowserGlobalContext
]
