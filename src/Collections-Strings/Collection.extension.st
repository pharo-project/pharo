Extension { #name : 'Collection' }

{ #category : '*Collections-Strings' }
Collection >> asCommaString [

	"Return collection printed as 'a, b, c' "

	"#('a' 'b' 'c') asCommaString >>> 'a, b, c'"
	"#('a') asCommaString >>> 'a'"
	"#() asCommaString >>> ''"
	"'foo' asCommaString >>> 'f, o, o'"
	"(10 to: 25 by: 5) asCommaString >>> '10, 15, 20, 25'"

	^ String streamContents: [ :s | self asStringOn: s delimiter: ', ' ]
]

{ #category : '*Collections-Strings' }
Collection >> asCommaStringAnd [

	"Return collection printed as 'a, b and c' "

	"#( 'a' 'b' 'c') asCommaStringAnd >>> 'a, b and c'"
	"#('a') asCommaStringAnd >>> 'a'"
	"#() asCommaStringAnd >>> ''"
	"'foo' asCommaStringAnd >>> 'f, o and o'"
	"(10 to: 25 by: 5) asCommaStringAnd >>> '10, 15, 20 and 25'"

	^String streamContents: [:s | self asStringOn: s delimiter: ', ' last: ' and ']
]

{ #category : '*Collections-Strings' }
Collection >> asMultilineString [

	^ String streamContents: [ :stream |
		  self
			  do: [ :each | stream nextPutAll: each asString ]
			  separatedBy: [ stream nextPut: Character cr ] ]
]
