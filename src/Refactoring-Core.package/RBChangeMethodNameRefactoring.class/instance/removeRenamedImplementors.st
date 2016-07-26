transforming
removeRenamedImplementors
	oldSelector = newSelector
		ifTrue: [ ^ self ].
	self implementors 
		do: [ :each | each removeMethod: oldSelector ]