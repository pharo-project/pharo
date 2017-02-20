private - updating
generateCacheFor: anObject
	| valuesCache |
	
	valuesCache := OrderedDictionary new.
	((self basicVariableNodesFor: anObject) do: [ :assoc | 
		valuesCache 
			at: assoc key 
			put: (self generateHashFor: assoc value) ]).
	
	^ valuesCache