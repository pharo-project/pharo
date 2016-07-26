pragma-selectors
raises: aClassOrClassName
	<gtExamplePragma>
	<description: 'This example is expected to raise the given exception. The example will fail if the exception is not raised.'>
	
	self classNamed: aClassOrClassName do: [ :class | 
		self exceptions add: class ]