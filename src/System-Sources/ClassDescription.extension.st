Extension { #name : #ClassDescription }

{ #category : #'*System-Sources' }
ClassDescription >> logMethodSource: aText forMethod: aCompiledMethod inProtocol: protocol withStamp: changeStamp [

	aCompiledMethod
		putSource: aText
		class: self
		protocol: protocol
		withStamp: changeStamp
		priorMethod: (self compiledMethodAt: aCompiledMethod selector ifAbsent: [  ])
]
