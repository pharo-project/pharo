tests-perform
testPerformChangeClass
	| change |
	change := changes defineClass: self class name , ' subclass: #' , self changeMock name , '
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , self class category , ''''.
	self perform: change do: [ 
		self assert: change changeClass superclass = self class.
		self assert: change changeClass instVarNames isEmpty ].
	self assert: change changeClass superclass = Object.
	self assert: change changeClass instVarNames notEmpty.
	self assert: change definedClass = self changeMock