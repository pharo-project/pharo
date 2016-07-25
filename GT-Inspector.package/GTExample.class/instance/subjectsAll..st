pragma-selectors
subjectsAll: aString
	<gtExamplePragma>
	
	self substrings: aString do: [ :each | 
		self subject: each ]