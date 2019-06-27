Extension { #name : #ClyFindClassCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyFindClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyPackageContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyFindClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $f meta for: ClyGlobalContextOfFullBrowser 
]
