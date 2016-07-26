enumerating
intersection: aCollection
	"Answer the set theoretic intersection of two collections.
	Optimized version for Sets where no intermediate Set is necessary"

	| outputSet |
	
	outputSet := self class new.
	
	aCollection do: [ :each|
		((self includes: each) and: [(outputSet includes: each) not])
				ifTrue: [ outputSet add: each]].
		
	^ outputSet