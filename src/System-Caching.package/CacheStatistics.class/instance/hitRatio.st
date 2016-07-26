accessing
hitRatio
	"Return the ratio of hits against total calls I received.
	This will be a number between 0 and 1.
	When I am empty, return 0."
	
	| total |
	total := hits + misses.
	total = 0 ifTrue: [ ^ 0 ].
	^ hits / total