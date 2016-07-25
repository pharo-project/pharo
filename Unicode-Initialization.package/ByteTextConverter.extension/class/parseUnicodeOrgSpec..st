*Unicode-Initialization
parseUnicodeOrgSpec: url
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