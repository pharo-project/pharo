private
askForNodeSelectionFrom: nodes
	
	
	^ UIManager default 
		enterOrRequestFrom: (nodes collect: [:node | (node respondsTo: #selector )ifTrue:[node selector] ifFalse:[node name]])
		values: nodes 
		lines: #() 
		title: 'Which code do you want to select?' translated