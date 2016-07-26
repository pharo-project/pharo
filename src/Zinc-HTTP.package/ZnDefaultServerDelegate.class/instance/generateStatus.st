private
generateStatus
	"Generate an HTML page with the state of the server"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'ZnServer Status' do: [
			html tag: #p do: [ html << 'I am '; print: ZnCurrentServer value ].
			html tag: #p with: self systemVersionInfo.
			html tag: #pre with: self vmStats.
			html tag: #h1 with: 'Processes'.
			html tag: #ol do: [ 
				self processes do: [ :each |
					html tag: #li do: [ 
						html 
							str: each name; space; nextPut: $[; print: each priority; nextPut: $]; 
							tag: #br; str: each printString ] ] ] ] ]