enumerating
intersection: aCollection
	"Answer the set theoretic intersection of two collections."

	| set outputSet |
	
	set := self asSet.
	outputSet := Set new.
	
	aCollection do: [ :each|
		((set includes: each) and: [(outputSet includes: each) not])
			ifTrue: [ 
				outputSet add: each]].
		
	^ self species withAll: outputSet asArray