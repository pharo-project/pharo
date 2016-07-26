initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [(node selector == #add: or: [node selector == #addAll:]) and: [node isDirectlyUsed]]}'
		do: [ :node :answer | node ]