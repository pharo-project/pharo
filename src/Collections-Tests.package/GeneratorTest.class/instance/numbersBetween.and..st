generators
numbersBetween: aStartInteger and: aStopInteger
	"Yields the nubmers between aStartInteger and aStopInteger."
	
	^ Generator on: [ :generator |
		aStartInteger to: aStopInteger 
			do: [ :value | generator yield: value ] ]