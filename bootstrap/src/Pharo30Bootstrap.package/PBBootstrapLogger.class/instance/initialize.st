logging-Deprecated
initialize

	super initialize.
	queue := AtomicSharedQueue new.
	logging := false.