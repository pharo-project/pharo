Extension { #name : 'SycRemovePackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRemovePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation removalFor: ClyFullBrowserPackageContext
]
