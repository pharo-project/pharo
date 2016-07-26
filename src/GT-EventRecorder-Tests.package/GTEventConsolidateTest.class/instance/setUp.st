running
setUp
	super setUp.
	data := { 42. 'string'. Float pi }.
	consolidate := GTEventConsolidate new.
	collector1 := GTEventCollector new.
	collector2 := GTEventCollector new.
	collector3 := GTEventCollector new.
	collectors := Array with: collector1 with: collector2 with: collector3.
	