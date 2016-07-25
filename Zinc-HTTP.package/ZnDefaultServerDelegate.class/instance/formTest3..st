responses
formTest3: request
	| contents filename contentType page |
	contents := filename := contentType := ''.
	(request hasEntity and: [ request contentType matches: ZnMimeType multiPartFormData ])
		ifTrue: [ 
			(request entity partNamed: #file ifNone: [ nil ]) 
				ifNotNil: [ :part |
					filename := part fileName.
					contents := part contents.
					contentType := part  contentType.
					contentType isBinary ifTrue: [ contents := contents hex ] ] ].
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Form Test 3' do: [ 
			html 
				tag: #form 
				attributes: #(action 'form-test-3' 'accept-charset' 'utf-8' 
									enctype 'multipart/form-data' method POST) 
				do: [ 
					html 
						str: 'File'; space;
						tag: #input attributes: #(type file name file); space;
						tag: #input attributes: #(type submit) ];
				tag: #p do: [ html str: 'filename = '; str: filename ];
				tag: #p do: [ html str: 'content-type = '; str: contentType asString ];
				tag: #p do: [ html str: 'contents = '; str: contents asString ] ] ].
	^ ZnResponse ok: (ZnEntity html: page)