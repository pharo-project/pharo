pragma-selectors
dependsClass: aClassOrClassName selector: aSelector
	<gtExamplePragma>
	<description: 'This example depends on the example declared by the given class and selector. Any dependency will be performed before this example is performed. Order of declaration is important'>
	
	self classNamed: aClassOrClassName do: [ :class | 
		children add: (self proxyForClass: class selector: aSelector) ]