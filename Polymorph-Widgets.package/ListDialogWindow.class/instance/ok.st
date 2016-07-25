actions
ok

	(acceptNewEntry and: [ list isEmpty ]) ifTrue: [ 
		self answer: self searchString ].
	
	self answer ifNil: [ self searchAccept: self searchString ].
	
	self 
		cancelled: false;
		delete