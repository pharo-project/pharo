Extension { #name : #ClassDescription }

{ #category : #'*System-Sources' }
ClassDescription >> logMethodSource: aText forMethod: aCompiledMethod inCategory: aProtocol withStamp: changeStamp [

	aCompiledMethod
		putSource: aText
		class: self
		category: aProtocol
		withStamp: changeStamp
		priorMethod: (self compiledMethodAt: aCompiledMethod selector ifAbsent: [  ])
]
