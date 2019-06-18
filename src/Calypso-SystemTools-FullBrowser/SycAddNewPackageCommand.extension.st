Extension { #name : #SycAddNewPackageCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddNewPackageCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyPackageContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycAddNewPackageCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $k meta, $p meta for: ClyGlobalContextOfFullBrowser  
]
