testing
isEmpty
	"Return true when I contain no elements, false otherwise."

	^ head isNil and: [ tail isNil ]