system integration
packageMatchingExtensionName: aString includingClass: aClass
	| package categoryName |
	
	categoryName := aString.
	package := self packageMatchingExtensionName: categoryName.
	[ package includesClass: aClass ] 
		whileFalse: [ 
			(categoryName includes: $-)
				ifFalse: [ self error: 'Class not categorized!' ].
				
			categoryName := categoryName copyUpToLast: $-.
			package := self packageMatchingExtensionName: categoryName ].
		
	^ package