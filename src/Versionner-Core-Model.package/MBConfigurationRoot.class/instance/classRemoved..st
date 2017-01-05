updating
classRemoved: anEvent

	(anEvent classRemoved originalName asString beginsWith: 'ConfigurationOf')
		ifTrue: [ self respondToEventFor: anEvent classRemoved withOperation: #removed ]