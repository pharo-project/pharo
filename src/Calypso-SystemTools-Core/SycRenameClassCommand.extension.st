Extension { #name : 'SycRenameClassCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand class >> browserShortcutActivation [
"	<classAnnotation>"
	"Disabled to make the rename shortcut trigger the new solution."
	
	^ClyShortcutActivation renamingFor: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycRenameClassCommand class >> sourceCodeShortcutActivation [
"	<classAnnotation>"
	"Disabled to make the rename shortcut trigger the new solution."

	^ClyShortcutActivation renamingFor: ClySourceCodeContext
]
