testing
isAtStart
	"Answer true if receiver points to the start of text"

	^ self span prev isAtStart and: [ self spanPosition = 0 ]