updating
methodModified: anEvent

	(anEvent methodAffected class name asString beginsWith: 'ConfigurationOf')
		ifTrue: [  self respondToEventFor: anEvent methodAffected withOperation: #modified ]