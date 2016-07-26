running
setUp
	super setUp.
	bundle1 := GTEventBundle new 
		url: 'http://localhost/gt/events1' asZnUrl; category: #c1; data: #[1 2 3]; yourself.
	bundle2 := GTEventBundle new 
		url: 'http://localhost/gt/events1' asZnUrl; category: #c1; data: #[4 5 6]; yourself.
	bundle3 := GTEventBundle new 
		url: 'http://localhost/gt/events1' asZnUrl; category: #c2; data: #[7 8 9]; yourself.
	multiBundle := GTEventMultiBundle new.
