pragma-selectors
raisesAny: aString
	<gtExamplePragma>
	<description: 'This example is expected to raise any of the given exceptions. The example will fail if none of the exceptions is raised.'>
	
	self substrings: aString do: [ :each | 
		self raises: each ]