initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
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
		do: [ :node :answer | node ]