api-building
addAllFromPragma: pragma target: target

	self fromSpec: (PragmaMenuBuilder 
		pragmaKeyword: pragma
		model: target) menuSpec