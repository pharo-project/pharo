editor access
handleEdit: editBlock
	| result |

	textMorph editor 
		selectFrom: selectionInterval first 
		to: selectionInterval last;
		model: model.  "For, eg, evaluateSelection"
	textMorph handleEdit: [result := editBlock value].   "Update selection after edit"
	self scrollSelectionIntoView.
	self textChanged.
	^ result