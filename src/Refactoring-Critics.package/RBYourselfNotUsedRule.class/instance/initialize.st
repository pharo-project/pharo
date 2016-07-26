initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node | node parent isUsed not} yourself'
		do: [ :node :answer | node ]