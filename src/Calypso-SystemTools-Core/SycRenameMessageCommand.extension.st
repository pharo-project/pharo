Extension { #name : 'SycRenameMessageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> methodShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation renamingFor: ClyMethod asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameMessageCommand class >> sourceCodeShortcutActivation [

	<classAnnotation>
	^ ClyShortcutActivation by: $r meta for: ClyMethodSourceCodeContext
]
