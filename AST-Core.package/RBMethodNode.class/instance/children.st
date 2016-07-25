accessing
children
	^ OrderedCollection new
		addAll: self arguments;
		addAll: self pragmas;
		add: self body;
		yourself