instance creation
assign: aModel to: anInstVarName

	widgets 
		at: anInstVarName
		put: aModel.
		
	aModel owner: self