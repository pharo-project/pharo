convenience
streamContents: block
	"Execute block with a ZnHtmlOutputStream as argument to generate HTML, returning the resulting string"
	
	"ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Hello World' do: [ 
			html tag: #p with: 'Hello World & Universe !' ] ]"
	
	^ String streamContents: [ :out |
			block value: (self on: out) ]