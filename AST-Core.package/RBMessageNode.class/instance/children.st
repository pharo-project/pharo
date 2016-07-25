accessing
children
	^(OrderedCollection with: self receiver)
		addAll: self arguments;
		yourself