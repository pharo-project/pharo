instance creation
justSendsSuper
	^ self new
		matchesAnyMethodOf: #(
			'`@method: `@args ^ super `@method: `@args' 
			'`@method: `@args super `@method: `@args')
		do: [ :node :answer | true ];
		yourself