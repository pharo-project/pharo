updating
classModified: anEvent

	(anEvent classAffected name asString beginsWith: 'ConfigurationOf')
		ifTrue: [ self respondToEventFor: anEvent classAffected withOperation: #modified ]