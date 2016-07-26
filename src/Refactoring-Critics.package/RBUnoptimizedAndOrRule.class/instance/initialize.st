initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'(`@a and: `@b) and: `@c'
			'(`@a or: `@b) or: `@c' )
		do: [ :node :answer | node ]