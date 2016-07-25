initialization
initialize
	super initialize.
	self matcher 
		matches: '`{:node | node isMessage and: [node hasParentheses not and: [#(#+ #-) includes: node selector]]} * `@C'
		do: [ :node :answer | node ]