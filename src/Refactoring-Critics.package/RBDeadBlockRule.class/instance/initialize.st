initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node | node isBlock and: [node isUsed not]}'
		do: [ :node :answer | node ]