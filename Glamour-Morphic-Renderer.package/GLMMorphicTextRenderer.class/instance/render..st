rendering
render: aPresentation
	textModel := self textModelFor: aPresentation.
	textMorph := self textMorphForModel: textModel.	"this is a horrible hack: 
	because we cannot get the update of when the text changes from the morph,
	we get the reference to the mutable text object :) :)"
	textModel text: textMorph text.
	textMorph setSelection: textModel selection.	"Without setting the selection again 
	the smalltalkcode presentation shows no selection. Seems to be cause by the font: 
	message send when creating a GLMPluggableTextMorph"
	self installActionsOnModel: textModel fromPresentation: aPresentation.
	self installKeystrokeActionsOnMorph: textMorph textMorph fromPresentation: aPresentation.
	textMorph announcer
		when: GLMSelectedTextChanged
		do: [ :ann | 
			aPresentation announcer
				suspendAllWhile: [ 
					aPresentation selectionInterval: ann interval.
					aPresentation selectedText: ann selectedText ] ].
	textMorph announcer when: GLMTextChanged do: [ :ann | aPresentation suspendAllWhile: [ aPresentation text: ann text ] ].
	self registerEventsForPresentation: aPresentation.
	^ textMorph