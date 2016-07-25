responses
formTest2: request
	| input page |
	input := 'input'.
	(request hasEntity and: [ request contentType matches: ZnMimeType applicationFormUrlEncoded  ])
		ifTrue: [ input := request entity at: #input ifAbsent: [ 'input' ] ].
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Form Test 2' do: [ 
			html 
				tag: #form 
				attributes: #(action 'form-test-2' 'accept-charset' 'utf-8' method POST) 
				do: [ 
					html 
						str: 'Input'; space;
						tag: #input attributes: { #type. #input. #name. #input. #value. input }; space;
						tag: #input attributes: #(type submit); 
						str: 'for input'; space; tag: #em with: input ] ] ].
	^ ZnResponse ok: (ZnEntity html: page)