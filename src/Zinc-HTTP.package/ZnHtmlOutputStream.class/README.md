I am ZnHtmlOutputStream. I wrap another character write stream to offer a richer API for generating correct HTML markup.

See https://en.wikipedia.org/wiki/HTML

My streaming protocol contains the traditional write stream operations. These are raw and do not do any conversions/escaping.

My html protocols contains a rich API for generating correct HTML. 

String streamContents: [ :out | | html |
	html := ZnHtmlOutputStream on: out.
	html html5.
	html tag: #html do: [ 
		html tag: #body do: [
			html tag: #div class: #main do: [
				html tag: #p with: 'Hello World & Universe !'.
				html tag: #hr.
				html 
					tag: #em 
					attributes: #(class big id 1 disable nil) 
					with: 'The END' ] ] ] ].
	
ZnHtmlOutputStream streamContents: [ :html |
	html page: 'Hello World' do: [ 
		html tag: #div class: #main do: [
			html tag: #p with: 'Hello World & Universe !' ] ] ]

Part of Zinc HTTP Components.