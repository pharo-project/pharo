Extension { #name : #TraitDescription }

{ #category : #'*System-Sources' }
TraitDescription >> logMethodSource: aText forMethod: aCompiledMethod inCategory: category withStamp: changeStamp [
	aCompiledMethod
		putSource: aText
		class: self
		category: category
		withStamp: changeStamp 
		priorMethod: (self
					compiledMethodAt: aCompiledMethod selector
					ifAbsent: [])
]
