querying
revparse: aString 
	self withRepoDo: [ :repo | 
		^ (repo revparse: aString) asIcebergObjectInRepository: self
	]