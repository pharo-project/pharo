utilties
generateByteToUnicodeSpec: url
	"Return the formatted source code for an array mapping 
	the top 128 byte to unicode values from a Unicode.org url"
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'."

	| mapping |
	mapping := self parseUnicodeOrgSpec: url.  
	^ String streamContents: [ :stream |
		stream tab; << '"'; << 'self generateByteToUnicodeSpec: '; print: url; << '"'; cr; cr; tab; << '^ #('.
		(self top128FromUnicodeSpec: mapping) doWithIndex: [ :each :index |
			index - 1 \\ 8 = 0 ifTrue: [ stream cr; tab ].
			each isNil
				ifTrue: [ stream print: nil; space ]  
				ifFalse: [ stream << '16r' << (each printPaddedWith: $0 to: 4 base: 16); space ] ].
		stream nextPut: $); cr ]