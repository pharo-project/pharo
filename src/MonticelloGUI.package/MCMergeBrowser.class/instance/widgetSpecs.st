morphic ui
widgetSpecs
	"ToolBuilder doesn't know about innerButtonRow. Made explicit here."

	^#(
		((buttonRow) (0 0 1 0) (0 0 0 30))
		((listMorph:selection:menu: list selection methodListMenu:) (0 0 1 0.4) (0 30 0 0))
		((buttonRow: #((Keep chooseRemote 'keep the selected change' selectionIsConflicted)
		  (Reject chooseLocal 'reject the selected change' selectionIsConflicted))) (0 0.4 1 0.4) (0 0 0 32))
		((textMorph: text) (0 0.4 1 1) (0 32 0 0))
		)
