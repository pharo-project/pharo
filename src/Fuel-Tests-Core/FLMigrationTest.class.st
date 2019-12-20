"
SUnit tests for migrations
"
Class {
	#name : #FLMigrationTest,
	#superclass : #FLClassSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLMigrationTest >> redefined: aClass with: instanceVariableNames [

	| name |
	name := aClass name.
	self removeFromSystem: aClass.
	"In this case we need to create a class with notifications"
	^ (self withNotificationsNewClassWithInstanceVariableNames: instanceVariableNames) renameSilently: name.
	
]

{ #category : #'tests-manual' }
FLMigrationTest >> testBadDestinationVariableRename [
	
	| pointClass aPoint pointClassName |
	pointClass := self newClassWithInstanceVariableNames: 'x y'.
	pointClassName := pointClass name.
	aPoint := pointClass new.
	
	self materializer 
		migrateClassNamed: pointClassName 
		variables: {('x' -> 'posX')}.
		
	self serialize: aPoint.
	self should: [ self materialized ] raise: Error. 
	
	self flag: #pharoTodo "Assert an specific materialization error"
]

{ #category : #'tests-automatic' }
FLMigrationTest >> testChangeInSuperclass [
	"Tests that serializer tolarates when there is a change in the superclass between serialization and materialization"

	| aClass aClassSubclass instance materializedInstance |
	aClass := self newClassWithInstanceVariableNames: 'a b c' superclass: Object.
	aClassSubclass := self newClassWithInstanceVariableNames: 'd e' superclass: aClass.

	instance := aClassSubclass new.
	instance instVarNamed: 'a' put: $A.
	instance instVarNamed: 'b' put: $B.
	instance instVarNamed: 'c' put: $C.
	instance instVarNamed: 'd' put: $D.
	instance instVarNamed: 'e' put: $E.

	self serialize: instance.
	aClass removeInstVarNamed: 'a'.
	aClass removeInstVarNamed: 'b'.
	aClass addInstVarNamed: 'x'.
	aClassSubclass superclass: aClass.

	materializedInstance := self materialized.

	self assert: $D equals: (materializedInstance instVarNamed: 'd').
	self assert: $E equals: (materializedInstance instVarNamed: 'e').
	self should: [ materializedInstance instVarNamed: 'a' ] raise: Error.
	self should: [ materializedInstance instVarNamed: 'b' ] raise: Error.
	self assert: $C equals: (materializedInstance instVarNamed: 'c').
	self assert: (materializedInstance instVarNamed: 'x') isNil
]

{ #category : #'tests-manual' }
FLMigrationTest >> testClassAndVariableRename [
	| pointClass aPoint resultPoint pointClassName |
	pointClass := self newClassWithInstanceVariableNames: 'x y'.
	pointClassName := pointClass name.
	aPoint := pointClass new.
	aPoint instVarNamed: 'x' put: 7.
	aPoint instVarNamed: 'y' put: 11.

	self serialize: aPoint.
	pointClass renameSilently: (pointClassName , 'Renamed') asSymbol.
	pointClass := self redefined: pointClass with: 'posY posX'.

	self materializer migrateClassNamed: pointClassName toClass: pointClass variables: {('x' -> 'posX') . ('y' -> 'posY')}.

	resultPoint := self materialized.

	self assert: (resultPoint instVarNamed: 'posX') equals: 7.
	self assert: (resultPoint instVarNamed: 'posY') equals: 11
]

{ #category : #'tests-manual' }
FLMigrationTest >> testClassRename [
	| pointClass aPoint resultPoint pointClassName |
	pointClass := self newClassWithInstanceVariableNames: 'x y'.
	pointClassName := pointClass name.
	aPoint := pointClass new.
	aPoint instVarNamed: 'x' put: 7.
	aPoint instVarNamed: 'y' put: 11.

	self serialize: aPoint.
	pointClass renameSilently: (pointClassName , 'Renamed') asSymbol.

	self materializer migrateClassNamed: pointClassName toClass: pointClass.

	resultPoint := self materialized.

	self assert: (resultPoint instVarNamed: 'x') equals: 7.
	self assert: (resultPoint instVarNamed: 'y') equals: 11
]

{ #category : #'tests-automatic' }
FLMigrationTest >> testFormatFixedToVariable [
	"Tests that serializer can tolerate a format change from a *fixed* class to *weak/variable*.
	Notice, however, that the other way around (variable to fixed) doesn't work so far"

	| materialized |
	self serialize: FLPair new.
	materialized := self during: [ self materialized ] rename: FLPair as: FLWeakClassMock.
	self assert: materialized size equals: 0
]

{ #category : #'tests-automatic' }
FLMigrationTest >> testSuperclassChange [
	"Tests that serializer tolarates when the superclass changed between serialization and materialization"

	| aClass aClassSubclass instance materializedInstance anotherSuperclass |
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

	self assert: $D equals: (materializedInstance instVarNamed: 'd').
	self assert: $E equals: (materializedInstance instVarNamed: 'e').
	self should: [ materializedInstance instVarNamed: 'a' ] raise: Error.
	self should: [ materializedInstance instVarNamed: 'b' ] raise: Error.
	self should: [ materializedInstance instVarNamed: 'c' ] raise: Error.
	self assert: (materializedInstance instVarNamed: 'x') isNil
]

{ #category : #'tests-automatic' }
FLMigrationTest >> testVariableInsertion [
	"Tests that serializer tolarates when there is a new instance variable on materialization"

	| pairClass aPair resultPair |
	pairClass := self newClassWithInstanceVariableNames: 'left right'.
	aPair := pairClass new.
	aPair instVarAt: 1 put: $A.
	aPair instVarAt: 2 put: $B.

	self serialize: aPair.
	self redefined: pairClass with: 'left middle right'.
	resultPair := self materialized.

	self assert: $A equals: (resultPair instVarAt: 1).
	self assert: nil equals: (resultPair instVarAt: 2).
	self assert: $B equals: (resultPair instVarAt: 3)
]

{ #category : #'tests-automatic' }
FLMigrationTest >> testVariableOrderChange [
	"Tests that serializer tolarates when the order in the instance variables changed between serialization and materialization"

	| pairClass aPair resultPair |
	pairClass := self newClassWithInstanceVariableNames: 'left right'.
	aPair := pairClass new.
	aPair instVarAt: 1 put: $A.
	aPair instVarAt: 2 put: $B.

	self serialize: aPair.
	self redefined: pairClass with: 'right left'.
	resultPair := self materialized.

	self assert: $B equals: (resultPair instVarAt: 1).
	self assert: $A equals: (resultPair instVarAt: 2)
]

{ #category : #'tests-automatic' }
FLMigrationTest >> testVariableRemoved [
	"Tests that serializer tolarates when an instance variable is missing on materialization"

	| pairClass aPair resultPair |
	pairClass := self newClassWithInstanceVariableNames: 'left right'.
	aPair := pairClass new.
	aPair instVarAt: 1 put: $A.
	aPair instVarAt: 2 put: $B.

	self serialize: aPair.
	self redefined: pairClass with: 'right'.
	resultPair := self materialized.

	self assert: $B equals: (resultPair instVarAt: 1)
]

{ #category : #'tests-manual' }
FLMigrationTest >> testVariableRename [
	| pointClass aPoint resultPoint pointClassName |
	pointClass := self newClassWithInstanceVariableNames: 'x y'.
	pointClassName := pointClass name.
	aPoint := pointClass new.
	aPoint instVarNamed: 'x' put: 7.
	aPoint instVarNamed: 'y' put: 11.

	self serialize: aPoint.
	self redefined: pointClass with: 'posY posX'.

	self materializer migrateClassNamed: pointClassName variables: {('x' -> 'posX') . ('y' -> 'posY')}.

	resultPoint := self materialized.

	self assert: (resultPoint instVarNamed: 'posX') equals: 7.
	self assert: (resultPoint instVarNamed: 'posY') equals: 11
]
