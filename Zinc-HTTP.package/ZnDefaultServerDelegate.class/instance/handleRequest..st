public
handleRequest: request
	| prefix prefixHandler |
	prefix := request uri isSlash
		ifTrue: [ prefixMap at: '/' ifAbsent: [ nil ] ]
		ifFalse: [ request uri firstPathSegment ].
	prefixHandler := prefixMap at: prefix ifAbsent: [ nil ].
	prefixHandler isNil
		ifFalse: [ 
			^ prefixHandler isSymbol
				ifTrue: [ self perform: prefixHandler with: request ]
				ifFalse: [ prefixHandler value: request ] ].
	^ ZnResponse notFound: request uri