parsing
contentsForMember: member
	^[(member contentStreamFromEncoding: 'utf8') text contents] on: ZnInvalidUTF8
		do: [:exc | 
			"Case of legacy encoding, presumably it is latin-1.
			But if contents starts with a null character, it might be a case of WideString encoded in UTF-32BE"
			| str |
			str := (member contentStreamFromEncoding: 'latin1') text.
			exc return: ((str peek = Character null and: [ str size \\ 4 = 0 ])
				ifTrue: [WideString fromByteArray: str contents asByteArray]
				ifFalse: [str contents])]