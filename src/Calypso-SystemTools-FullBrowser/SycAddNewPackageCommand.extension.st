Extension { #name : 'SycAddNewPackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAddNewPackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemFor: ClyFullBrowserPackageContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAddNewPackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $k meta, $p meta for: ClyFullBrowserGlobalContext
]
