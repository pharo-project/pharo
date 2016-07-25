tests
testInlineMethodForSuperSend
	| refactoring |
	model := Smalltalk compiler evaluate: self inlineMethodTestData.
	(model classNamed: #RBRenameVariableChange) removeMethod: #executeNotifying:.
	refactoring := RBInlineMethodRefactoring 
				model: model
				inline: (self convertInterval: (102 to: 131)
						for: ((model classNamed: #RBRenameInstanceVariableChange) 
								sourceCodeFor: #executeNotifying:))
				inMethod: #executeNotifying:
				forClass: (model classNamed: #RBRenameInstanceVariableChange).
	self executeRefactoring: refactoring.
	self 
		assert: ((model classNamed: #RBRenameInstanceVariableChange) 
				parseTreeFor: #executeNotifying:) = (RBParser 
							parseMethod: 'executeNotifying: aBlock 
									| undo undos undo1 |
									self addNewVariable.
									self copyOldValuesToNewVariable.
									undos := changes collect: [:each | each executeNotifying: aBlock].
									undo1 := self copy.
									undo1 changes: undos reverse.
									undo := undo1.
									self removeOldVariable.
									^undo')