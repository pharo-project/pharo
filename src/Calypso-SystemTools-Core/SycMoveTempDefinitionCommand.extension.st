Extension { #name : 'SycMoveTempDefinitionCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycMoveTempDefinitionCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClyMethodSourceCodeContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycMoveTempDefinitionCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>

	^CmdShortcutActivation moveFor: ClyMethodSourceCodeContext
]
