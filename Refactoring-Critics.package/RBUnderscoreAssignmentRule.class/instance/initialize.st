initialization
initialize
	super initialize.
	self rewriteRule 
		replace: '`var := ``@object' with: '`var := ``@object'
		when: [ :node | node assignmentOperator = '_' ]