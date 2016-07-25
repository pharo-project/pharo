view
open
	| window editor bar |
	window := (StandardWindow labelled: 'Code and text editor') model: self.
	bar := window newRow: {
			window 
				newCheckboxFor: self 
				getSelected: #codeEditing 
				setSelected: #codeEditing:
				label: 'Code editing' 
				help: 'Checked for code editing' }.
	editor := window newTextEditorFor:  self getText:  #code setText: #code: getEnabled: nil. 
	window addMorph: (window newColumn: {bar. editor})
		fullFrame: LayoutFrame identity.
	^ window openInWorld