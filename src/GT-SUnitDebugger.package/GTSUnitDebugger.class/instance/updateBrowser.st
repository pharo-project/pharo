updating
updateBrowser

	super updateBrowser.
	"The transmission will select the context of the test case. 
	This should only happen the first time.
	Should find another solution so this method doesn;t have to be overridden"
	((self browser paneNamed: #stack) port: #selection) value: self session interruptedContext