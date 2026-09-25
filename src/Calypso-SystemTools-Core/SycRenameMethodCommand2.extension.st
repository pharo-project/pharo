Extension { #name : 'SycRenameMethodCommand2' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMethodCommand2 class >> methodMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 1.7  for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMethodCommand2 class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMethodCommand2 class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMethodCommand2 class >> sourceCodeShortcutActivation [

	<classAnnotation>
	^ CmdShortcutActivation by: $r meta for: ClyMethodSourceCodeContext
]
