actions
accept
	self username isEmptyOrNil ifTrue: [ ^ UIManager default alert: 'user.name is required.' ].
	self email isEmptyOrNil ifTrue: [ ^ UIManager default alert: 'user.email is required.' ].	
	
	acceptBlock 
		value: self username 
		value: self email
		value: self isGlobal.
	
	self window delete.