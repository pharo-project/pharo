*Unicode-Initialization
generateByteToUnicodeSpec: url
	"Return the formatted source code for an array mapping 
	the top 128 byte to unicode values from a Unicode.org url"
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'."

	| mapping |
	mapping := self parseUnicodeOrgSpec: url.  
	^ String streamContents: [ :stream |
		stream tab; << '"'; << self name; << ' generateByteToUnicodeSpec: '; print: url; << '"'; cr; cr; tab; << '^ #('.
		128 to: 255 do: [ :each | | unicode |
			each \\ 8 = 0 ifTrue: [ stream cr; tab ].
			(unicode := mapping at: each ifAbsent: [ nil ]) 
				ifNil: [ stream print: nil; space ]  
				ifNotNil: [ stream << '16r' << (unicode printPaddedWith: $0 to: 4 base: 16); space ] ].
		stream nextPut: $); cr ]