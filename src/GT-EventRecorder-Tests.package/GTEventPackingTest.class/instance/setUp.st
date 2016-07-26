running
setUp
	super setUp.
	event1 := GTDummyEvent new data: 'string'.
	event2 := GTDummyEvent new data: 42.
	collector := GTEventCollector new.
	collector add: event1; add: event2.
	collector category: #category1.
	packing := self packingClass new.
	storage := FileSystem memory.
	file := storage workingDirectory / 'data'.
