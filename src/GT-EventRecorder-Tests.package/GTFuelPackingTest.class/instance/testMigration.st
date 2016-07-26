tests
testMigration
	| oldClass oldObject |
	oldClass := Object newAnonymousSubclass.
	oldObject := oldClass new.
	collector := GTEventCollector new.
	collector add: oldObject.
	bundle := packing pack: collector.
	announcement := packing unpack: bundle data.
	self should: [ announcement unpackedData ] raise: FLClassNotFound.