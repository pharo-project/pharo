updating
classRemoved: anEvent

	(anEvent classRemoved name asString beginsWith: 'ConfigurationOf')
		ifTrue: [ self respondToEventFor: anEvent classRemoved withOperation: #removed ]