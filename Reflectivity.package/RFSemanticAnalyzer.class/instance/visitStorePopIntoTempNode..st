visiting
visitStorePopIntoTempNode: aNode 
	| name var |
	name := aNode name.
	var := scope lookupVarForDeclaration: name.
	var	ifNil: [ 
			var := scope addTemp: name ].
	aNode binding: var.