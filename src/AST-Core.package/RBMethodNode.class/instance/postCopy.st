copying
postCopy
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ]).
	self pragmas: (self pragmas collect: [ :each | each copy ]).
	self body: self body copy