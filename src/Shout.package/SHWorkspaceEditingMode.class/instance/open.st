view
open
	| window editor |
	window := (StandardWindow labelled: 'Editor with Workspace like  syntax highlighting') model: self.
	editor := window newTextEditorFor:  self getText:  #code setText: #code: getEnabled: nil. 
	"Set-up the styler to be in workspace mode"
	editor styler workspace: self.
	window addMorph: editor frame: (0@0 extent: 1@1).
	^ window openInWorld