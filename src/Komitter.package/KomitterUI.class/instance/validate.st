api
validate

	self commitableEntities 
		ifEmpty: [ 
			tree takeKeyboardFocus.
			self inform: 'The selection should contain commitable entities'.
			^ false ].
		
	message accept.
	
	^ true