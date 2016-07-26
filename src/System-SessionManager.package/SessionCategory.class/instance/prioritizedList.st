accessing
prioritizedList
	^ priorityLists keys sorted 
		flatCollect: [ :index | priorityLists at: index ]