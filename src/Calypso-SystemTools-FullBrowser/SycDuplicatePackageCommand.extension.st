Extension { #name : 'SycDuplicatePackageCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycDuplicatePackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1.1 for: ClyFullBrowserPackageContext
]

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycDuplicatePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $c meta for: ClyFullBrowserPackageContext
]
