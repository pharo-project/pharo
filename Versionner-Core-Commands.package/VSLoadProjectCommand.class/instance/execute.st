execute
execute
	| mcRepository projectName configurationName  |
	mcRepository := MCTool newRepository.
	(mcRepository isNil or: [ mcRepository location isEmpty ])
		ifTrue: [ ^ self ].

	projectName := ([ mcRepository project ]
		on: Error
		do: [ UIManager default request: 'Enter the name of the project to load' ]) value.
	(projectName isNil or: [ projectName isEmpty ]) ifTrue: [ ^ self ].
	
	configurationName := self configurationNameForProject: projectName.
	[ Gofer new
		url: mcRepository location;
		package: configurationName;
		load ]
		on: Error
		do: [ :ex | 
			self inform: 'Cannot find the repository or the configuration. Please check'.
			^ self ].