building
updateBindings: aBinding of: newClass
	newClass methods do: [ :e | e classBinding: aBinding ]