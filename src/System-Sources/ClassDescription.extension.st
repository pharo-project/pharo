Extension { #name : #ClassDescription }

{ #category : #'*System-Sources' }
ClassDescription >> logMethodSource: aText forMethod: aCompiledMethod inProtocolNamed: protocolName withStamp: changeStamp [

	aCompiledMethod
		putSource: aText
		class: self
		protocol: protocolName
		withStamp: changeStamp
		priorMethod: (self compiledMethodAt: aCompiledMethod selector ifAbsent: [  ])
]
