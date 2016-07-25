updating
classAdded: anEvent

	(anEvent classAdded name asString beginsWith: 'ConfigurationOf')
		ifTrue: [ self respondToEventFor: anEvent classAdded withOperation: #added ]