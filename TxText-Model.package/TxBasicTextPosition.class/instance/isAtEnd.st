testing
isAtEnd
	"Answer true if receiver points to the end of text"
	^ self span next isAtEnd and: [ self spanPosition = self span size ]