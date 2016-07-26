tests
testMigration2
	| oldClass oldObject |
	oldClass := Object newAnonymousSubclass.
	oldObject := oldClass new.
	collector := GTEventCollector new.
	collector add: oldObject.
	bundle := packing pack: collector.
	packing addMigration: [ :aMaterializer |
		aMaterializer migrateClassNamed: oldClass name asSymbol toClass: GTDummyEvent ].
	announcement := packing unpack: bundle data.
	announcement migrationRules: packing migrations.
	unpackedData := announcement unpackedData.
	self assert: unpackedData isCollection.
	self assert: unpackedData size equals: 1.
	self assert: unpackedData anyOne class equals: GTDummyEvent.