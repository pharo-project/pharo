"
SUnit tests for hooked serialization using fuel
"
Class {
	#name : 'FLHookedSubstitutionTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLHookedSubstitutionTest >> testAvoidRecursion [
	| result original | 
	original := FLClassWithRecursiveSubstitution new index: 1.
	
	"Fail fast so the image doesn't crash if we end up in an endless loop"
	result := [ self resultOfSerializeAndMaterialize: original ]
		valueWithin: 1 second
		onTimeout: [ self fail ].
	
	self assert: result index equals: 2
]

{ #category : 'tests' }
FLHookedSubstitutionTest >> testClassWithCachedValueByNil [

	| aClassWithCachedValue result original |
	aClassWithCachedValue := self classFactory silentlyMake: [ :aBuilder | aBuilder slotsFromString: 'cache' ].
	self classFactory
		silentlyCompile: 'cache ^cache' in: aClassWithCachedValue;
		silentlyCompile: 'cache: x cache := x' in: aClassWithCachedValue;
		silentlyCompile: '
			fuelAccept: aVisitor  
				^cache 
					ifNil: [ super fuelAccept: aVisitor ]
					ifNotNil: [
						aVisitor
							visitSubstitution: self
							by: (self copy cache: nil)]'
		in: aClassWithCachedValue.

	original := aClassWithCachedValue new
		            cache: 5;
		            yourself.

	result := self resultOfSerializeAndMaterialize: original.

	self assert: result cache isNil
]

{ #category : 'tests' }
FLHookedSubstitutionTest >> testObjectByProxyThatBecomesItsContent [
	"Tests a substitution of an object by a proxy that becomes another object on materialization."

	| aProxyClass result |
	aProxyClass := self classFactory silentlyMake: [ :aBuilder | aBuilder slotsFromString: 'someState' ].
	self classFactory
		silentlyCompile: 'initialize  someState := 5@1' in: aProxyClass;
		silentlyCompile: 'fuelAccept: aVisitor
			^ aVisitor
				visitSubstitution: self
				by: (' , FLProxyThatBecomesItsContent name , ' newWith: someState)'
		in: aProxyClass.

	result := self resultOfSerializeAndMaterialize: aProxyClass new.

	self assert: 5 @ 1 equals: result
]

{ #category : 'tests' }
FLHookedSubstitutionTest >> testProxyByTarget [

	| aProxyClass result original |
	aProxyClass := self classFactory silentlyMake: [ :aBuilder | aBuilder sharedVariablesFromString: 'Target' ].
	self classFactory
		silentlyCompile: 'target: x  Target := x' in: aProxyClass;
		silentlyCompile: 'fuelAccept: aVisitor
			^ aVisitor
				visitSubstitution: self
				by: Target'
		in: aProxyClass.

	original := aProxyClass new
		            target: 5;
		            yourself.

	result := self resultOfSerializeAndMaterialize: original.

	self assert: 5 equals: result
]

{ #category : 'tests' }
FLHookedSubstitutionTest >> testProxyByTargetAnalisysIsPropagated [

	| aProxyClass result pair original |
	aProxyClass := self classFactory silentlyMake: [ :aBuilder | aBuilder sharedVariablesFromString: 'Target' ].
	self classFactory
		silentlyCompile: 'target: x  Target := x' in: aProxyClass;
		silentlyCompile: 'fuelAccept: aVisitor
			^ aVisitor
				visitSubstitution: self
				by: Target'
		in: aProxyClass.

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

{ #category : 'tests' }
FLHookedSubstitutionTest >> testProxyByTargetInsideObjectAndAnalisysIsPropagated [

	| aProxyClass result original pair pairRoot |
	aProxyClass := self classFactory silentlyMake: [ :aBuilder | aBuilder sharedVariablesFromString: 'Target' ].
	self classFactory
		silentlyCompile: 'target: x  Target := x' in: aProxyClass;
		silentlyCompile: 'fuelAccept: aVisitor
			^ aVisitor
				visitSubstitution: self
				by: Target'
		in: aProxyClass.

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

{ #category : 'tests' }
FLHookedSubstitutionTest >> testProxyInsideObjectByTarget [

	| aProxyClass result original pair |
	aProxyClass := self classFactory silentlyMake: [ :aBuilder | aBuilder sharedVariablesFromString: 'Target' ].
	self classFactory
		silentlyCompile: 'target: x  Target := x' in: aProxyClass;
		silentlyCompile: 'fuelAccept: aVisitor
			^ aVisitor
				visitSubstitution: self
				by: Target'
		in: aProxyClass.

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

{ #category : 'tests' }
FLHookedSubstitutionTest >> testSpecialObjectsArray [
	| result |
	result := self resultOfSerializeAndMaterialize: Smalltalk specialObjectsArray.
	
	self assert: result isNil
]

{ #category : 'tests' }
FLHookedSubstitutionTest >> testSubstituteByItself [
	| aRecursiveClass result | 
	aRecursiveClass := self classFactory silentlyNewClass .
	self classFactory
		silentlyCompile: 'fuelAccept: aMapper  
			^ aMapper 
				visitSubstitution: self by: self 
				onRecursionDo: [ super fuelAccept: aMapper ]'
		in: aRecursiveClass.

	"Fail fast so the image doesn't crash if we end up in an endless loop"
	result := [ self resultOfSerializeAndMaterialize: aRecursiveClass new ]
		valueWithin: 1 second
		onTimeout: [ self fail ].
	
	"shouldn't lead to endless loop"
	self assert: result class identicalTo: aRecursiveClass
]

{ #category : 'tests' }
FLHookedSubstitutionTest >> testTransientByNil [
	| result aClassOfTransientObjects | 
	aClassOfTransientObjects := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'fuelAccept: aVisitor
			^ aVisitor visitSubstitution: self by: nil'
		in: aClassOfTransientObjects.

	result := self resultOfSerializeAndMaterialize: aClassOfTransientObjects new.
	
	self assert: result isNil
]
