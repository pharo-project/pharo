initialization
initialize

	super initialize.
	
	items := OrderedCollection new 
			"SortedCollection sortBlock: [ :a :b | 
		(a timeIfAbsent: [ DateAndTime today ]) >= (b timeIfAbsent: [ DateAndTime today ]) ]"