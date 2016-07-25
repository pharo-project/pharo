current-context
bestNodeForSelection
	| root |
	
	currentEditor isWorkspace 
		ifTrue: [root := RBParser parseFaultyExpression: self currentText]
		ifFalse:  [root := RBParser parseFaultyMethod: self currentText]. 
	^ root bestNodeFor: self textSelection .	