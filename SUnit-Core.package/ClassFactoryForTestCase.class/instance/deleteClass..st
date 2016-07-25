cleaning
deleteClass: aClass
	self createdClasses remove: aClass.
	self delete: aClass.