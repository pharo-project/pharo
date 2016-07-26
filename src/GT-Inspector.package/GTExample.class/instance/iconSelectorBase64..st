pragma-selectors
iconSelectorBase64: aSelector
	<gtExamplePragma>
	<description: 'Attach an icon to this example. The selector must return a base64 string'>
	
	self initializeIconWith: [ 
		self iconBase64: (self provider perform: aSelector) ]