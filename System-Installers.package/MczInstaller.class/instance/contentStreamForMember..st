private
contentStreamForMember: member
	^[(member contentStreamFromEncoding: 'utf8') text] on: ZnInvalidUTF8
		do: [:exc | 
			"Case of legacy encoding, presumably it is latin-1 and we do not need to do anything
			But if contents starts with a null character, it might be a case of WideString encoded in UTF-32BE"
			| str |
			str := (member contentStreamFromEncoding: 'latin1') text.
			(str peek = Character null and: [ str size \\ 4 = 0 ])
				ifTrue: [str := (WideString fromByteArray: str contents asByteArray) readStream].
			exc return: str]