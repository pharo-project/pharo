Extension { #name : 'SycInspectVariableValueCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycInspectVariableValueCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation
		by: PharoShortcuts current inspectItShortcut for: ClySourceCodeContext
]
