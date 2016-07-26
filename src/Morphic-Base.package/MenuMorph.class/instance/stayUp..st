accessing
stayUp: aBoolean

	stayUp := aBoolean.
	
	titleMorph ifNotNil: [ titleMorph updatePinForm ].