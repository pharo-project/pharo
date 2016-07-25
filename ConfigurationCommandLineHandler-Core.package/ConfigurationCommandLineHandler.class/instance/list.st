actions
list
	(self hasConfiguration or: [ self hasRepositoryUrl ])
		ifTrue: [ self listConfigurationDetails ]
		ifFalse: [ self listConfigurations ]