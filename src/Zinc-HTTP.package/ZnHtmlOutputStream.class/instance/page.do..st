html
page: title do: block
	"Write out a standard page using title, then execute block"
	
	"ZnHtmlOutputStream streamContents: [ :html | html page: 'Hello' do: [ html tag: #p with: 'World' ] ]"

	self html5; tag: #html do: [ 
		self
			tag: #head do: [ self tag: #title with: title ];
			tag: #body do: [ 
				self tag: #h1 with: title.
				block value ] ]