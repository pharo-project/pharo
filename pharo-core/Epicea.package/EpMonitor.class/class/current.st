accessing
current

	current ifNil: [
		current := self new ].
	
	^ current 
	