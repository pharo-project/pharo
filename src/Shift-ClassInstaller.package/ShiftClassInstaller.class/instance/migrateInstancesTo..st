as yet unclassified
migrateInstancesTo: newClass
	| oldObjects newObjects |
	oldObjects := oldClass allInstances.
	newObjects := oldObjects collect: [ :e | self copyObject: e to: newClass ].

	oldObjects elementsForwardIdentityTo: newObjects copyHash: true