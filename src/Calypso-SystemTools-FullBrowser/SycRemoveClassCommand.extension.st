Extension { #name : #SycRemoveClassCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyClassContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyClassContextOfFullBrowser 
]
