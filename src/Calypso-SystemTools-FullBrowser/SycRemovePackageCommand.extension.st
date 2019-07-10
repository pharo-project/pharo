Extension { #name : #SycRemovePackageCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemovePackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyPackageContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemovePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyPackageContextOfFullBrowser
]
