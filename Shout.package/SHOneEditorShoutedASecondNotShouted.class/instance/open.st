view
open
	| window codeEditor commentEditor |
	window := (StandardWindow labelled: 'A styled editor and a not styled one for the same model') model: self.
	codeEditor := window newTextEditorFor:  self getText:  #code setText: #code: getEnabled: nil. 
	commentEditor := window newTextEditorFor:  self getText:  #comment setText: #comment: getEnabled: nil. 
	"now, comment must not be highlighted: just send #styled: to the editor with false as argument"
	commentEditor styled: false.
	window addMorph: codeEditor frame: (0@0 extent: 1@0.5).
	window addMorph: commentEditor frame: (0@0.5 extent: 1@1).
	^ window openInWorld