pragma-selectors
iconThemeSelector: aSelector
	<gtExamplePragma>
	<description: 'Attach an icon to this example. The selector must be implemented by the currently active theme'>
	
	self initializeIconWith: [ 
		form := (Smalltalk ui icons perform: aSelector) ]