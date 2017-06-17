building
compose 
	self tabulator with: [ :browser |
		self configureBrowser: browser.
		self composeBrowserOn: browser  ]