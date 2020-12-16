"
SUnit tests for hooked serialization using fuel
"
Class {
	#name : #FLHookedSubstitutionTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #tests }
FLHookedSubstitutionTest >> testAvoidRecursion [
	| result original | 
	original := FLClassWithRecursiveSubstitution new index: 1.
	
	result := [ self resultOfSerializeAndMaterialize: original ]
		valueWithin: 1 second
		onTimeout: [ self fail ].
	
	self assert: result index equals: 2
]

{ #category : #tests }
FLHookedSubstitutionTest >> testClassWithCachedValueByNil [

	| aClassWithCachedValue result original | 
	aClassWithCachedValue := (self newSubclassOf: Object instanceVariableNames: 'cache' classVariableNames: '') 
		duringTestCompileSilently: 'cache ^cache';
		duringTestCompileSilently: 'cache: x cache := x';
		duringTestCompileSilently: '
			fuelAccept: aVisitor  
				^cache 
					ifNil: [ super fuelAccept: aVisitor ]
					ifNotNil: [ aVisitor visitSubstitution: self by: (self copy cache: nil)]';
		yourself.

	original := aClassWithCachedValue new cache: 5; yourself.

	result := self resultOfSerializeAndMaterialize: original.
	
	self assert: result cache isNil.
]

{ #category : #tests }
FLHookedSubstitutionTest >> testObjectByProxyThatBecomesItsContent [
	"Tests a substitution of an object by a proxy that becomes another object on materialization."

	| aProxyClass result |
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'someState' classVariableNames: '')
		duringTestCompileSilently: 'initialize  someState := 5@1';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: (FLProxyThatBecomesItsContent newWith: someState)';
		yourself.

	result := self resultOfSerializeAndMaterialize: aProxyClass new.

	self assert: 5 @ 1 equals: result
]

{ #category : #tests }
FLHookedSubstitutionTest >> testProxyByTarget [
	| aProxyClass result original |
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '')
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	original := aProxyClass new
		target: 5;
		yourself.

	result := self resultOfSerializeAndMaterialize: original.

	self assert: 5 equals: result
]

{ #category : #tests }
FLHookedSubstitutionTest >> testProxyByTargetAnalisysIsPropagated [
	| aProxyClass result pair original |
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '')
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	pair := FLPair new
		left: 6;
		right: 'foo';
		yourself.

	original := aProxyClass new
		target: pair;
		yourself.

	result := self resultOfSerializeAndMaterialize: original.

	self assert: result left equals: 6.
	self assert: result right equals: 'foo'
]

{ #category : #tests }
FLHookedSubstitutionTest >> testProxyByTargetInsideObjectAndAnalisysIsPropagated [
	| aProxyClass result original pair pairRoot |
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '')
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	pair := FLPair new
		left: 6;
		right: 'foo'.

	original := aProxyClass new
		target: pair;
		yourself.

	pairRoot := FLPair new
		left: original;
		yourself.

	result := self resultOfSerializeAndMaterialize: pairRoot.

	self assert: result left left equals: 6.
	self assert: result left right equals: 'foo'.
	self assert: result right isNil
]

{ #category : #tests }
FLHookedSubstitutionTest >> testProxyInsideObjectByTarget [
	| aProxyClass result original pair |
	aProxyClass := (self newSubclassOf: Object instanceVariableNames: 'target' classVariableNames: '')
		duringTestCompileSilently: 'target: x  target := x';
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: target';
		yourself.

	original := aProxyClass new
		target: 5;
		yourself.

	pair := FLPair new
		left: original;
		right: 'foo'.

	result := self resultOfSerializeAndMaterialize: pair.

	self assert: result left equals: 5.
	self assert: result right equals: 'foo'
]

{ #category : #tests }
FLHookedSubstitutionTest >> testSubstituteByItself [
	| aRecursiveClass result | 
	aRecursiveClass := (self newSubclassOf: Object instanceVariableNames: '' classVariableNames: '') 
		duringTestCompileSilently: 'fuelAccept: aMapper  
			^aMapper 
				visitSubstitution: self by: self 
				onRecursionDo: [super fuelAccept: aMapper]';
		yourself.

	result := [ self resultOfSerializeAndMaterialize: aRecursiveClass new ]
		valueWithin: 1 second
		onTimeout: [ self fail ].
	
	"shouldn't lead to endless loop"
	self assert: result class identicalTo: aRecursiveClass
]

{ #category : #tests }
FLHookedSubstitutionTest >> testTransientByNil [
	| result aClassOfTransientObjects | 
	aClassOfTransientObjects := self newClass
		duringTestCompileSilently: 'fuelAccept: aVisitor  ^aVisitor visitSubstitution: self by: nil';
		yourself.

	result := self resultOfSerializeAndMaterialize: aClassOfTransientObjects new.
	
	self assert: result isNil
]
