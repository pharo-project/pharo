accessing
removeClassNamed: className
	self removeClassDefinitionName: className.
	self removeAllMethodsFromClassNamed: className. 
	self removeClassTagsForClassNamed: className.