accessing text
updateTextWith: aStringOrText
	self
		handleEdit: [ 
			self
				beEditableWhile: [ 
					aStringOrText = self text
						ifFalse: [ 
							self selectAll.							
							self editor replaceSelectionWith: aStringOrText.
							self deselect ] ] ]