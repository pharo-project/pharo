Extension { #name : #SycRemoveClassCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation  
		byItemOf: CmdRefactorMenuGroup  
		order: 3
		for: ClyClass asCalypsoItemContext 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycRemoveClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyClassContextOfFullBrowser 
]
