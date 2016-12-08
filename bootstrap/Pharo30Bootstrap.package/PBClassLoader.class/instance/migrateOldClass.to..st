registration
migrateOldClass: oldClass to: newClass

	| oldClassMetaclass |
	
	"We migrate the class pools"
	oldClass asClassMirror classPool isNilObject ifFalse: [
		newClass asClassMirror classPool: oldClass asClassMirror classPool ].
	
	"We migrate the class instance variables"
	"A class has at least 14 instances variables (cf class description, behavior). Starting from 15, we get user defined class instances variables."
	15 to: oldClass fixedSize do: [ :index |
		newClass instanceVariableAtIndex: index put: (oldClass instanceVariableAtIndex: index).
	].

	"We become the old class and metaclass into the new ones"
	oldClassMetaclass := oldClass basicClass.
	oldClass becomeInto: newClass.
	oldClassMetaclass target == Metaclass
		ifFalse:[ oldClassMetaclass becomeInto: newClass basicClass ] 