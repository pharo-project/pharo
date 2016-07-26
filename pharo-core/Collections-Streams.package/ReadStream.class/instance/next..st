accessing
next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans
