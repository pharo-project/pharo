running
setUp
	browser := 
		self newBrowser
			addFilter: self andFilter;
			yourself.

	view := 
		browser view
			pageSize: self pageSize;
			yourself.

	browser refresh.