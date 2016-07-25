removing
removeCollector: aGTEventCollector 
	| removedCollector |
	removedCollector := collectors remove: aGTEventCollector.
	collectors size = 0 ifTrue: [ self deactivateDelivery ].
	^ removedCollector