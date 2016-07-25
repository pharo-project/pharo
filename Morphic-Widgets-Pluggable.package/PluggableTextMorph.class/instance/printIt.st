menu commands
printIt
	| oldEditor printString |
	textMorph editor
		selectFrom: selectionInterval first to: selectionInterval last;
		model: model.	"For, eg, evaluateSelection"
	textMorph
		handleEdit: [ 
			(oldEditor := textMorph editor)
				evaluateSelectionAndDo: [ :result | 
					printString := [ result printString ]
						on: Error
						do: [ '<error in printString: try ''Inspect it'' to debug>' ].
					selectionInterval := oldEditor selectionInterval.
					textMorph installEditorToReplace: oldEditor.
					textMorph handleEdit: [ oldEditor afterSelectionInsertAndSelect: printString ].
					selectionInterval := oldEditor selectionInterval.
					textMorph editor selectFrom: selectionInterval first to: selectionInterval last.
					self scrollSelectionIntoView ] ]