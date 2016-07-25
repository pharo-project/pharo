accessing
addColumn: aBlockOrSymbol 
	 
	| newColumn cell | 
	cell := aBlockOrSymbol isSymbol 
		ifTrue: 
			[self browser 
				addNewPaneNamed: aBlockOrSymbol. 
			(GLMCustomColumn new) 
				id: aBlockOrSymbol; 
				browser: self browser] 
		ifFalse: 
			[newColumn := GLMCustomColumn new browser: self browser. 
			aBlockOrSymbol value: newColumn. 
			newColumn]. 
	self children addLast: cell. 
	^cell