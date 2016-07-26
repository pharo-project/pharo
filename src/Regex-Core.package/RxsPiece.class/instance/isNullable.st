testing
isNullable
	"A piece is nullable if it allows 0 matches. 
	This is often handy to know for optimization."

	^min = 0 or: [atom isNullable]