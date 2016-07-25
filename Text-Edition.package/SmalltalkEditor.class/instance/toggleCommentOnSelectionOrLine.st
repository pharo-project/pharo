shortcuts
toggleCommentOnSelectionOrLine
	| originalHasSelection selection selectionInterval originalSelectionInterval quote    modifiedSelection newSelectionLastPosition numberOfDoubleQuoteBeforeCaret |
				
	selectionInterval := originalSelectionInterval := self selectionInterval.
	originalHasSelection := self hasSelection.
			
	originalHasSelection ifFalse: [
		selectionInterval := self string encompassLine: originalSelectionInterval.	
		self selectInvisiblyFrom: selectionInterval first to: selectionInterval last.
	].
					
	selection := self string copyFrom: selectionInterval first to: selectionInterval last.
		
	numberOfDoubleQuoteBeforeCaret := 0.
	selection withIndexDo: [ :char :i | 
		((i + selectionInterval first) < originalSelectionInterval first and: [ char = $" ]) ifTrue: [ 
		numberOfDoubleQuoteBeforeCaret := numberOfDoubleQuoteBeforeCaret  + 1
	]].
				
	quote := selection trimLeft first.
	quote = $" ifTrue: [ 
		modifiedSelection := selection asUncommentedCode.
		numberOfDoubleQuoteBeforeCaret := (((numberOfDoubleQuoteBeforeCaret - 1) // 2) + 1) negated.
		newSelectionLastPosition := (selection size - modifiedSelection size) negated.
	] ifFalse: [ 
		modifiedSelection := selection asComment.
		numberOfDoubleQuoteBeforeCaret := numberOfDoubleQuoteBeforeCaret + 1.
		newSelectionLastPosition := modifiedSelection size - selection size.
	].
			
	self replaceSelectionWith: modifiedSelection.
		
	originalHasSelection 
		ifFalse: [
			self selectAt: (originalSelectionInterval first + numberOfDoubleQuoteBeforeCaret).
		] ifTrue: [ 
			self selectInterval: ((originalSelectionInterval first) to: (originalSelectionInterval last + newSelectionLastPosition))
		]