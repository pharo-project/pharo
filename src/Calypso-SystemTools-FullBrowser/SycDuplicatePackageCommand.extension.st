Extension { #name : 'SycDuplicatePackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycDuplicatePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation by: $c meta for: ClyFullBrowserPackageContext
]
