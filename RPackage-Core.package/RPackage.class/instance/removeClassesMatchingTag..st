class tags
removeClassesMatchingTag: aTag
	
	aTag classes do: [ :class | self removeClassDefinition: class ]