Extension { #name : 'SycRenameClassCommand2' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand2 class >> browserShortcutActivation [
	<classAnnotation>
	
	^ClyShortcutActivation renamingFor: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand2 class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation renamingFor: ClySourceCodeContext
]
