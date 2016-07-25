tests
testSmallPage
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Small' do: [ 
			html tag: #p with: 'This is a small HTML document' ] ].
	self assert: result equals: '<!DOCTYPE html><html><head><title>Small</title></head><body><h1>Small</h1><p>This is a small HTML document</p></body></html>'