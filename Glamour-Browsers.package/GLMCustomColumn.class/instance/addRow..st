accessing
addRow: aBlockOrSymbol 
	 
	| newRow cell | 
	cell := aBlockOrSymbol isSymbol 
		ifTrue: 
			[self browser 
				addNewPaneNamed: aBlockOrSymbol. 
			(GLMCustomRow new) 
				id: aBlockOrSymbol; 
				browser: self browser] 
		ifFalse: 
			[newRow := GLMCustomRow new browser: self browser. 
			aBlockOrSymbol value: newRow. 
			newRow]. 
	self children addLast: cell. 
	^cell