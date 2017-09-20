copying
postCopy
	super postCopy.
	presentations ifNotNil: [ :arg | 
		presentations := presentations collect: [ :each | each copy ] ].
	arrangement ifNotNil: [ :arg | 
		arrangement := arrangement copy.
		arrangement composite: self ]