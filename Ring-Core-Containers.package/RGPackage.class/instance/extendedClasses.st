accessing
extendedClasses
	"Retrieves the classes holding the extension methods and returns a dictionary as well"
	|  classesOfExt |
	classesOfExt := Dictionary new.
	
	self extensionMethods
		do:[ :mth| (classesOfExt includesKey: mth parentName)
					ifFalse:[ classesOfExt at: mth parentName put: mth parent ] ].
	
	^classesOfExt