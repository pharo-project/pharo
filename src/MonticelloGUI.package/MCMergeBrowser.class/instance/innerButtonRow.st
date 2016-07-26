actions
innerButtonRow
	^ self buttonRow:
		#((Keep chooseRemote 'keep the selected change' selectionIsConflicted)
		  (Reject chooseLocal 'reject the selected change' selectionIsConflicted))