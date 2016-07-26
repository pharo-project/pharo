instructions
returnInstVar: index 
	bytes ifEmpty: [
		lastSpecialReturn := Message selector: #returnInstVar: argument: index].
	self pushInstVar: index.
	self returnTop.
