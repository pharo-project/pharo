tests
test22addAll
	| link5 link6 link7 link8 listToBeAdded |
	link5 := Link new.
	link6 := Link new.
	link7 := Link new.
	link8 := Link new.
	
	list
		add: link1;
		add: link2;
		add: link3;
		add: link4.
		
	listToBeAdded := LinkedList new.
	listToBeAdded
		add: link5;
		add: link6;
		add: link7;
		add: link8.
		
	list addAll: listToBeAdded.
	
	self should: [(list at: 1) == link1].
	self should: [(list at: 2) == link2].
	self should: [(list at: 3) == link3].
	self should: [(list at: 4) == link4].
	self should: [(list at: 5) == link5].
	self should: [(list at: 6) == link6].
	self should: [(list at: 7) == link7].
	self should: [(list at: 8) == link8].