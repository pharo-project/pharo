testing
isIfNilIfNotNil: msgNode
	msgNode receiver isMessage
		ifFalse: [ ^ false ].
	msgNode receiver selector = #==
		ifFalse: [ ^ false ].
	^ msgNode receiver arguments first value isNil