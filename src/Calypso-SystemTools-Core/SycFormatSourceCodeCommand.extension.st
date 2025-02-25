Extension { #name : 'SycFormatSourceCodeCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycFormatSourceCodeCommand class >> methodEditorShorcutActivation [

	<classAnnotation>
	^ CmdShortcutActivation
		by: $f meta shift
		for: ClyTextEditorContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycFormatSourceCodeCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 100000 for: ClyMethodSourceCodeContext
]
