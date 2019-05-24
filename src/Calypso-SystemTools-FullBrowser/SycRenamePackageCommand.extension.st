Extension { #name : #SycRenamePackageCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRenamePackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyPackageContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRenamePackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyPackageContextOfFullBrowser
]
