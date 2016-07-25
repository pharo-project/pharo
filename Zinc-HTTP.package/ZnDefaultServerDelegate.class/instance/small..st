responses
small: request
	| page |
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: #Small do: [ 
			html tag: #p with: 'This is a small HTML document' ] ].
	^ ZnResponse ok: (ZnEntity html: page)