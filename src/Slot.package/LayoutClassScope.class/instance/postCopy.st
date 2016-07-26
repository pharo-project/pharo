copying
postCopy
	parentScope := parentScope copy.
	1 to: self size do: [ :index | 
		self at: index put: (self at: index) copy ]