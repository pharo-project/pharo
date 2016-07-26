running
setUp
	super setUp.
	bundle := GTEventBundle new.
	data := #[1 2 3 4].
	category := #category1.
	url := 'http://localhost/gt/events' asZnUrl.
	