tests
testUpdateBrowserTitle
	browser title: 'something'.
	browser update.
	self assert: window label equals: 'something'