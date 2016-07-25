tests-perform
testPerformChangeMetaclass
	| change |
	change := changes defineClass: self changeMock name , ' class 
	instanceVariableNames: '''''.
	self perform: change do: [ self assert: change changeClass class instVarNames isEmpty ].
	self assert: change changeClass class instVarNames notEmpty.
	self assert: change definedClass = self changeMock class