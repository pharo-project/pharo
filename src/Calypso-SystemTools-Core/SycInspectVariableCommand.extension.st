Extension { #name : #SycInspectVariableCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycInspectVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycInspectVariableCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation 
		by: PharoShortcuts current inspectItShortcut for: ClySourceCodeContext
]
