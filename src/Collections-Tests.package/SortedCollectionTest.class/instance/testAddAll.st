tests - basic
testAddAll
	"self run: #testAddAll"
	"self debug: #testAddAll"
	
	| sorted2 sorted|
	sorted2 := SortedCollection new.
	sorted2 add: 'brochet'; add:'truitelle'.
	sorted := SortedCollection new.
	sorted addAll: sorted2.
	self assert: (sorted hasEqualElements: sorted2).
	 