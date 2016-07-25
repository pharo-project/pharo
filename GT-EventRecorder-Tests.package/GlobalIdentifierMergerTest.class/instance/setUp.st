running
setUp
	super setUp.
	existing := Dictionary new.
	stored := Dictionary new.
	merger := GlobalIdentifierMerger new.
	merger existing: existing; stored: stored