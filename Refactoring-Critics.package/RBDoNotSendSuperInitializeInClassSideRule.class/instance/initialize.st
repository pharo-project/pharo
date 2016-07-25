initialization
initialize
	super initialize.
	self rewriteRule 
		replace: '``@.statBefore. super initialize. ``@.statAfter.' 
		with: '``@.statBefore. ``@.statAfter.' 
		when: [ :node | class isMeta and: [ node methodNode selector = #initialize ] ]