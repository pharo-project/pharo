environments
forClass: aClass selectors: selectorCollection 
	^(RBSelectorEnvironment 
		onMethods: selectorCollection
		forClass: aClass
		in: self)
		label: aClass name , '>>' 
					, (selectorCollection detect: [:each | true] ifNone: ['']);
		yourself