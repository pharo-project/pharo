adding/removing
removeMethod: aRGMethodDefinition

	self 
		removeElement: aRGMethodDefinition 
		from: (aRGMethodDefinition isExtension 
				ifTrue: [ self extensionMethods ]
				ifFalse:[ super methods ])