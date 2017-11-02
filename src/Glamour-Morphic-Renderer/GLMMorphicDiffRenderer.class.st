Class {
	#name : #GLMMorphicDiffRenderer,
	#superclass : #GLMMorphicWidgetRenderer,
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicDiffRenderer >> render: aPresentation [
	| textMorph |
	textMorph := (DiffMorph
		from: aPresentation displayValue first
		to: aPresentation displayValue last
		contextClass: aPresentation contextClassValue)
"			onKeyStrokeSend: #keystroke:from: to: textModel;
"			layoutFrame: (LayoutFrame identity);
			vResizing: #spaceFill;
			hResizing: #spaceFill;
			yourself.
"	self installActionsOnUI: textModel fromPresentation: aTextPresentation.
	aTextPresentation selectedTextBlock: [ textMorph textMorph editor selection ].
	aTextPresentation textBlock: [ textMorph textMorph editor text ].
"	^textMorph
]
