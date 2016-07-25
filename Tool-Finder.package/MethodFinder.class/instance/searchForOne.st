search
searchForOne
	"Look for and return just one answer"

	expressions := OrderedCollection new.
	self search: false.	"non-multi"
	^ expressions
			