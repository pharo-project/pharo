Extension { #name : 'SycRenameClassCommand2' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand2 class >> browserContextMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1 for: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand2 class >> browserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation renamingFor: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand2 class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClySourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand2 class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClySourceCodeContext
]
