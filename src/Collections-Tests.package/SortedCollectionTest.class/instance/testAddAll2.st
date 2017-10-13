tests - basic
testAddAll2
	"self run: #testAddAll2"
	"self debug: #testAddAll2"
	
	| sorted2 sorted|
	sorted2 := SortedCollection new.
	sorted2 add: 'brochet'; add:'truitelle'.
	sorted := SortedCollection new.
	sorted add: 'perche'.
	sorted addAll: sorted2.
	self assert: (sorted size = (sorted2 size + 1)).
	sorted2 do: 
			[ :each | self assert: (sorted includes: each)]
	 