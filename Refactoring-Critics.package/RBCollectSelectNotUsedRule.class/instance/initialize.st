initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [(#(#select: #collect: #reject:) includes: node selector) and: [node isUsed not]]}'
		do: [ :node :answer | node ]