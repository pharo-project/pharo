initialization
initialize
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher 
		matches: '^ ``@object'
		do: [ :node :answer | answer add: node value; yourself ]