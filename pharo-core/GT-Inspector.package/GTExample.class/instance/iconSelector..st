pragma-selectors
iconSelector: aSelector
	<gtExamplePragma>
	<description: 'Attach an icon to this example. The selector must return an icon form'>
	
	self initializeIconWith: [ 
		form := (self provider perform: aSelector) ]