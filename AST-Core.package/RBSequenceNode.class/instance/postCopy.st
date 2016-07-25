copying
postCopy
	super postCopy.
	self temporaries: (self temporaries collect: [ :each | each copy ]).
	self statements: (self statements collect: [ :each | each copy ])