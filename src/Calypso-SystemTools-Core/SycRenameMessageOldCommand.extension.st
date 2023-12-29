Extension { #name : 'SycRenameMessageOldCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageOldCommand class >> browserMenuOrder [
	^2
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageOldCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation
		byRootGroupItemOrder: self browserMenuOrder for: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageOldCommand class >> methodShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageOldCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageOldCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation renamingFor: ClyMethodSourceCodeContext
]
