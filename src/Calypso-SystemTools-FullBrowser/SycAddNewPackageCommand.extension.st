Extension { #name : 'SycAddNewPackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAddNewPackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $k meta, $p meta for: ClyFullBrowserGlobalContext
]
