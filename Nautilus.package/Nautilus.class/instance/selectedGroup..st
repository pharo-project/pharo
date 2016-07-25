accessing
selectedGroup: anObject
	selectedGroup := anObject.
	anObject
		ifNotNil: [ 
			self announcer announce: (NautilusGroupSelected group: anObject).
			self ui ifNotNil: [ self ui sourceTextModel announce: NautilusGroupSelected ] ]