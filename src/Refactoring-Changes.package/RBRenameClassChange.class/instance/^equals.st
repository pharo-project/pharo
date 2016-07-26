comparing
= aRenameClassChange 
	super class = aRenameClassChange class
		ifFalse: [ ^ false ].
	^oldName = aRenameClassChange oldName 
		and: [ newName = aRenameClassChange newName ]