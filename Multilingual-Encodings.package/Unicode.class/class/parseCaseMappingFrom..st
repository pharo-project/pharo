class initialization
parseCaseMappingFrom: stream
	"Parse the Unicode casing mappings from the given stream.
	Handle only the simple mappings"

	ToCasefold := IdentityDictionary new: 2048.
	ToUpper := IdentityDictionary new: 2048.
	ToLower := IdentityDictionary new: 2048.
	[ stream atEnd ]
		whileFalse: [ 
			| fields line srcCode dstCode |
			line := stream nextLine copyUpTo: $#.
			fields := line trimBoth findTokens: $;.
			(fields size > 2 and: [ #('C' 'S') includes: (fields at: 2) trimBoth ])
				ifTrue: [ 
					srcCode := Integer readFrom: (fields at: 1) trimBoth base: 16.
					dstCode := Integer readFrom: (fields at: 3) trimBoth base: 16.
					ToCasefold at: srcCode put: dstCode ] ].
	ToCasefold
		keysAndValuesDo: [ :k :v | 
			(self isUppercase: (self value: k))
				ifTrue: [ 
					"In most cases, uppercase letter are folded to lower case"
					ToUpper at: v ifAbsentPut: k.
					ToLower at: k ifAbsentPut: v ].
			(self isLowercase: (self value: k))
				ifTrue: [ 
					"In a few cases, two lower case letters are folded to the same lower case.
				We must find an upper case letter folded to the same letter"
					ToCasefold keys
						detect: [ :e | (self isUppercase: (self value: e)) and: [ (ToCasefold at: e) = v ] ]
						ifFound: [ :up | ToUpper at: k ifAbsentPut: up ] ] ]