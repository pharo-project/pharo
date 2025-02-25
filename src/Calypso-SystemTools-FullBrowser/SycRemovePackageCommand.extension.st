Extension { #name : 'SycRemovePackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRemovePackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyFullBrowserPackageContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycRemovePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation removalFor: ClyFullBrowserPackageContext
]
