updating
addDependent: anObject

	self announcer 
		when: MorphChanged , MorphChangedWithArguments
		send: #handleUpdate:
		to: anObject.
			
	^ anObject