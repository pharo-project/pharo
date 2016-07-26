updating
update

	self session 
		ifNil: [ self clearToolbar ] 
		ifNotNil: [ self rebuildToolbar ]
	