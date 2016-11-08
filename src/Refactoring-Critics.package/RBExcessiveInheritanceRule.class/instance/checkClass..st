running
checkClass: aClass
	(self basicCheck: aClass)
		ifTrue: [ result addClassAndMetaClassFor: aClass ]