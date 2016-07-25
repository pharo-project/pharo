lookup
lookupSelector: name

	Symbol hasInterned: name ifTrue: [ :sym | ^ sym].
	^ nil