pragma-selectors
depends: aSelector
	<gtExamplePragma>
	<description: 'This example depends on the example declared by the given selector. Any dependency will be performed before this example is performed. Order of declaration is important'>
	
	children add: (self proxyForSelector: aSelector)