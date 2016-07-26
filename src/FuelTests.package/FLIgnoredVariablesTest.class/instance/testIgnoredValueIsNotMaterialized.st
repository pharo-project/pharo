tests
testIgnoredValueIsNotMaterialized

	| anObject materializedInstances aClass |
	aClass := self newClassWithInstanceVariableNames: 'a'.
	aClass class duringTestCompileSilently: 'fuelIgnoredInstanceVariableNames ^#(a)'.
	anObject := aClass new
		instVarAt: 1 put: #A;
		yourself.

	self serialize: anObject.
	materializedInstances := self materializedObjects.

	self deny: (materializedInstances includes: #A)
