cleaning
cleanupProtocols: aWorkingCopy
	aWorkingCopy packageSet extensionClasses do: [ :class |
		(aWorkingCopy packageSet extensionCategoriesForClass: class) do: [ :category |
			(class organization listAtCategoryNamed: category) isEmpty
				ifTrue: [ class organization removeCategory: category ] ] ].
	aWorkingCopy packageSet classesAndMetaClasses do: [ :class |
		(aWorkingCopy packageSet coreCategoriesForClass: class) do: [ :category |
			(class organization listAtCategoryNamed: category) isEmpty
				ifTrue: [ class organization removeCategory: category ] ] ]