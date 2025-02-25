Extension { #name : 'SycRenameMessageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> methodMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 1.7  for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> sourceCodeShortcutActivation [

	<classAnnotation>
	^ CmdShortcutActivation by: $r meta for: ClyMethodSourceCodeContext
]
