text area attribute
changeAttribute: anAttribute
	"Change a TextAttribute  of the receiver. Emphasis, alignment..."

	self textArea
		beEditableWhile: [ 
			self textArea
				restoreSelectionIndexesAfter: [ 
					self textArea selectAll.
					self editor applyAttribute: anAttribute ] ].
	self textArea changed