Extension { #name : 'Symbol' }

{ #category : '*AST-Core' }
Symbol class >> readFrom: strm [
	"Symbol readFromString: '#abc'"

	strm peek = $# ifFalse: [
		self error: 'Symbols must be introduced by #' ].
	^ (OCParser parseLiterals: strm contents) first
]
