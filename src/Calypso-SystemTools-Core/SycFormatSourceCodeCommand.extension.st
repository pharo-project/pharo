Extension { #name : 'SycFormatSourceCodeCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycFormatSourceCodeCommand class >> methodEditorShorcutActivation [

	<classAnnotation>
	^ ClyShortcutActivation
		by: $f meta shift
		for: ClyTextEditorContext
]
