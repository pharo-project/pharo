initialization
initialize
	super initialize.
	self matchesAny: #(
			'[| `@temps | 
				`var := `@object.  
				`@.statements] 
						ensure: 
							[`var `@messages: `@args]'
			'[| `@temps | 
				`var := `@object.  
				`@.statements] 
						ifCurtailed: 
							[`var `@messages: `@args]' )