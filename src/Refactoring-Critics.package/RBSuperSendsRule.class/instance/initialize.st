initialization
initialize
	super initialize.
	self rewriteRule 
		replace: 'super `@message: ``@args' 
		with: 'self `@message: ``@args'
		when: [ :node | 
			(class withAllSubclasses 
				detect: [:each | each includesSelector: node selector]
				ifNone: [ nil ]) isNil ]