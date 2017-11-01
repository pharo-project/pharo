Extension { #name : #ByteTextConverter }

{ #category : #'*Unicode-Initialization' }
ByteTextConverter class >> generateByteToUnicodeSpec: url [
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
]

{ #category : #'*Unicode-Initialization' }
ByteTextConverter class >> parseUnicodeOrgSpec: url [
	"Parse and return a mapping from byte to unicode values from url."
	"self parseUnicodeOrgSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'."
	
	| mapping |
	mapping := Dictionary new: 256.
	url asZnUrl retrieveContents linesDo: [ :each |
		(each isEmpty or: [ each beginsWith: '#' ])
			ifFalse: [ | tokens hexReader |
				hexReader := [ :string | Integer readFrom: (string readStream skip: 2; yourself) base: 16 ].
				tokens := each findTokens: String tab. 
				(tokens last = '<control>' or: [ tokens last = '#UNDEFINED' ]) ifFalse: [
					mapping
						at: (hexReader value: tokens first)
						put: (hexReader value: tokens second) ] ] ].
	^ mapping 
]
