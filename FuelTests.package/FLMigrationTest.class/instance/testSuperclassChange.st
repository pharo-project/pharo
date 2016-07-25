tests-automatic
testSuperclassChange
	"Tests that serializer tolarates when the superclass changed between serialization and materialization"
	
	|  aClass aClassSubclass instance materializedInstance anotherSuperclass |	
	
	aClass := self newClassWithInstanceVariableNames: 'a b c' superclass: Object.
	aClassSubclass := self newClassWithInstanceVariableNames: 'd e' superclass: aClass.
	anotherSuperclass := self newClassWithInstanceVariableNames: 'x' superclass: Object.
	
	instance := aClassSubclass new.
	instance instVarNamed: 'a' put: $A.	
	instance instVarNamed: 'b' put: $B.	
	instance instVarNamed: 'c' put: $C.	
	instance instVarNamed: 'd' put: $D.	
	instance instVarNamed: 'e' put: $E.	
	
	self serialize: instance.
	aClassSubclass superclass: anotherSuperclass.
	materializedInstance := self materialized.

	self assert: $D = (materializedInstance instVarNamed: 'd').
	self assert: $E = (materializedInstance instVarNamed: 'e').
	self should: [materializedInstance instVarNamed: 'a'] raise: Error. 
	self should: [materializedInstance instVarNamed: 'b'] raise: Error. 
	self should: [materializedInstance instVarNamed: 'c'] raise: Error. 
	self assert: (materializedInstance instVarNamed: 'x') isNil.
	  
	
	