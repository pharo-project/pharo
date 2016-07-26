comparing
= aRenameVariableChange 
	self class = aRenameVariableChange class 
		ifFalse: [ ^ false ].
	^ className = aRenameVariableChange changeClassName 
		and: [ isMeta = aRenameVariableChange isMeta 
		and: [ oldName = aRenameVariableChange oldName 
		and: [ newName = aRenameVariableChange newName ] ] ]