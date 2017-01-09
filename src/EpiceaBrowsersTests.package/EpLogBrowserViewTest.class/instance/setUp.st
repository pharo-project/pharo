running
setUp
	super setUp.
	
	logBuilder := EpTestLogBuilder new.

	browser := 
		self newBrowser
			addFilter: self andFilter;
			yourself.

	view := 
		browser view
			pageSize: self pageSize;
			yourself.

	browser refresh.