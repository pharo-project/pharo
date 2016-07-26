pragma-selectors
subject: aClassOrClassName
	<gtExamplePragma>
	
	self classNamed: aClassOrClassName do: [ :class | 
		self subjects add: class ]