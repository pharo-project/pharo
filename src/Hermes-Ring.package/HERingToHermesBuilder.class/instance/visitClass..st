visiting
visitClass: aClass
	| aHEClass |
	
	aHEClass := HEClass new.
	self visitMethodContainer: aClass using: aHEClass.

	aHEClass className: aClass name.
	aHEClass superclass: (aClass superclass ifNil:'' ifNotNil:#name).
	aHEClass instanceVariables: (aClass instanceVariables joinUsing: ' ').
	aHEClass classInstancevariables: (aClass theMetaClass instanceVariables joinUsing: ' ').
	aHEClass classVariables: (aClass classPool keys joinUsing: ' ').
	aHEClass sharedPools: ((aClass sharedPools collect: #name) joinUsing: ' ').
	aHEClass layoutClass: aClass ficusClass layout realClassName.
	
	^ aHEClass.