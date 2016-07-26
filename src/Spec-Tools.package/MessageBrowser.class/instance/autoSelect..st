text selection
autoSelect: aSelector
	textModel
		readSelectionBlock:
			[ :text | 
			self selectedMessage ifNotNil:
					[ :msg | (self isMethodDefinition: msg)
						ifFalse: [ self intervalOf: aSelector inCommentText: text ] 
						ifTrue: [ self intervalOf: aSelector inCode: text of: msg ] ] ]