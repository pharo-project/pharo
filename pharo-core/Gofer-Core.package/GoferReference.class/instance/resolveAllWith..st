querying
resolveAllWith: aGofer
	"Answer a sorted collection of all resolved references within aGofer."

	^ aGofer allResolved select: [ :each | self matches: each ]