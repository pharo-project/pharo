initialization
initialize
	super initialize.
	self matcher 
		matches: '^`{:aNode | aNode isAssignment and: [(aNode whoDefines: aNode variable name) notNil]}'
		do: [ :node :answer | node ]