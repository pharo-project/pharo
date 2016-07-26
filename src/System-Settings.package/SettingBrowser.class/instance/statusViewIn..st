user interface
statusViewIn: aMorph
	| statusView |
	statusView := aMorph
				newTextEditorFor: self
				getText: #getDescription
				setText: #setDescription:.
	^ statusView
 