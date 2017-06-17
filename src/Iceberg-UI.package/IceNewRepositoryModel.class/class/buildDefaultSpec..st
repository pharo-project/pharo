specs
buildDefaultSpec: col
	col
		newRow: [:row | row
			add: #projectNameLabel width: 100;
			add: #projectNameInput ]
		height: self inputTextHeight.
		
	super buildDefaultSpec: col