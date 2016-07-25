initialization
initialize
	super initialize.
	self matcher 
		matches: '(`@a to: `@b) do: `@c'
		do: [ :node :answer | node ]