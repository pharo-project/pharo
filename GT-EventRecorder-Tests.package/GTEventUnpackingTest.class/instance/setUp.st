running
setUp
	super setUp.
	initialData := #('GT' 'Team' 2015).
	collector := GTEventCollector new.
	initialData do: [ :each | collector add: each ].
	unpacking := GTEventUnpacking default.