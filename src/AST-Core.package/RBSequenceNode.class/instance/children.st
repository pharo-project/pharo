accessing
children
	^(OrderedCollection new)
		addAll: self temporaries;
		addAll: self statements;
		yourself