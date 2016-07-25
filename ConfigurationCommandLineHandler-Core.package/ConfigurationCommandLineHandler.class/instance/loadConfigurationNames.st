actions
loadConfigurationNames
	| references configurations |
	
	references := self gofer allResolved.
		
	references := references 
			collect: #packageName 
			thenSelect: [ :name| name beginsWith: 'ConfigurationOf' ].
	
	^ references asSet asSortedCollection 