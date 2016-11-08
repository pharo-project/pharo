utilities api
allMatching: aPattern in: aCollection
	"Assumes that the collection is a collection of Strings"

	| matcher |
	
	matcher := self pattern: aPattern.

	^ aCollection select: [ :each | matcher matches: each ]