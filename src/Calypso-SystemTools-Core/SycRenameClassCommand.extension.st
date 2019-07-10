Extension { #name : #SycRenameClassCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameClassCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyClass asCalypsoItemContext  
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameClassCommand class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyClass asCalypsoItemContext 
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameClassCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClySourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycRenameClassCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClySourceCodeContext 
]
