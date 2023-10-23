Extension { #name : 'SycRenameMessage2Command' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessage2Command class >> browserMenuOrder [
	^2
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessage2Command class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation
		byRootGroupItemOrder: self browserMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessage2Command class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessage2Command class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessage2Command class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
