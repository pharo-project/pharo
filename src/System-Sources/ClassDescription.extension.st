Extension { #name : #ClassDescription }

{ #category : #'*System-Sources' }
ClassDescription >> logMethodSource: aText forMethod: aCompiledMethod inProtocol: aProtocol withStamp: changeStamp [

	aCompiledMethod
		putSource: aText
		class: self
		protocol: aProtocol
		withStamp: changeStamp
		priorMethod: (self compiledMethodAt: aCompiledMethod selector ifAbsent: [  ])
]
