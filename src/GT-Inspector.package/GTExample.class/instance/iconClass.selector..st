pragma-selectors
iconClass: aClassOrClassName selector: aSelector
	<gtExamplePragma>
	<description: 'Attach an icon to this example. The method described by the given class and selector must return a form-icon'>
	
	self initializeIconWith: [ 
		self classNamed: aClassOrClassName do: [ :class | 
			form := (class perform: aSelector) ] ]