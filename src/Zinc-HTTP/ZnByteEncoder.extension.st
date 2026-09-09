Extension { #name : 'ZnByteEncoder' }

{ #category : '*Zinc-HTTP' }
ZnByteEncoder class >> generateByteToUnicodeSpec: url [
	"Return the formatted source code for an array mapping
	the top 128 byte to unicode values from a Unicode.org url"
	"self generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'."

	| mapping |
	mapping := self parseUnicodeOrgSpec: url.
	^ String streamContents: [ :stream |
			stream tab; << '"'; << 'self generateByteToUnicodeSpec: '; print: url; << '"'; cr; cr; tab; << '^ #('.
			(self top128FromUnicodeSpec: mapping) doWithIndex: [ :each :index |
					index - 1 \\ 8 = 0 ifTrue: [ stream cr; tab ].
					each
						ifNil: [ stream print: nil; space ]
						ifNotNil: [ (stream << '16r') << (each printPaddedWith: $0 to: 4 base: 16); space ]
					].
			stream nextPut: $); cr ]
]

{ #category : '*Zinc-HTTP' }
ZnByteEncoder class >> newFromUrl: url [
	"Instanciate a new encoder directly from a Unicode.org url"
	"self newFromUrl: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'"
	"self newFromUrl: 'http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT'"

	| mapping spec tables |
	mapping := self parseUnicodeOrgSpec: url.
	spec := self top128FromUnicodeSpec: mapping.
	tables := self tablesFromSpec: spec.
	^ self new
		identifier: url;
		byteToUnicode: tables first;
		unicodeToByte: tables second;
		yourself
]

{ #category : '*Zinc-HTTP' }
ZnByteEncoder class >> parseUnicodeOrgSpec: url [
	"Parse and return a mapping from byte to unicode values from url."
	"Basic syntax: lines starting with # are comments, else first two fields are read as 0x hex values"
	"self parseUnicodeOrgSpec: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'."

	| mapping |
	mapping := Dictionary new: 256.
	url asZnUrl retrieveContents linesDo: [ :each |
		(each isEmpty or: [ each beginsWith: '#' ])
			ifFalse: [ | tokens hexReader |
				hexReader := [ :string | Integer readFrom: (string readStream skip: 2; yourself) base: 16 ].
				tokens := each findTokens: String tab.
				(tokens size < 3 or: [ tokens last = '<control>' or: [ tokens last = '#UNDEFINED' ] ]) ifFalse: [
					mapping
						at: (hexReader value: tokens first)
						put: (hexReader value: tokens second) ] ] ].
	^ mapping
]

{ #category : '*Zinc-HTTP' }
ZnByteEncoder class >> top128FromUnicodeSpec: mapping [
	"Return an array mapping the top 128 byte to unicode values from a Unicode.org specification map"

	^ Array new: 128 streamContents: [ :stream |
		128 to: 255 do: [ :each |
			stream nextPut: (mapping at: each ifAbsent: [ nil ]) ] ]
]
