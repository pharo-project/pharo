protocol
addEntry: aString date: d 
	| lists |
	lists := self listsForPrefix: aString.
	lists do: [:e | e
		addEntry: aString
		date: d] 