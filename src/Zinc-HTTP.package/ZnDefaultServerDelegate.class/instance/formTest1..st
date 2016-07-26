responses
formTest1: request
	| input page |
	input := request uri queryAt: #input ifAbsent: [ 'input' ].
	page := ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Form Test 1' do: [ 
			html 
				tag: #form 
				attributes: #(action 'form-test-1' 'accept-charset' 'utf-8' method GET) 
				do: [ 
					html 
						str: 'Input'; space;
						tag: #input attributes: { #type. #input. #name. #input. #value. input }; space;
						tag: #input attributes: #(type submit); 
						str: 'for input'; space; tag: #em with: input ] ] ].
	^ ZnResponse ok: (ZnEntity html: page)