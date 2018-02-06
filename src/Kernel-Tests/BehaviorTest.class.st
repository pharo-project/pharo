"
SUnit tests for Behavior
"
Class {
	#name : #BehaviorTest,
	#superclass : #ClassTestCase,
	#category : #'Kernel-Tests-Classes'
}

{ #category : #tests }
BehaviorTest >> sampleMessageWithFirstArgument: firstArgument "This is a comment intended to explain arg1"
   andInterleavedCommentBeforeSecondArgument: secondArgument [

	"This method is here to test a few utilities like formalParametersAt:"
	
	| thisIsAnUnusedTemp |
	thisIsAnUnusedTemp := self.
	^thisIsAnUnusedTemp
]

{ #category : #tests }
BehaviorTest >> testAllLocalCallsOn [
	
	self assert: (( Point allLocalCallsOn: #asPoint )  notEmpty).
	self assert: (( Point allLocalCallsOn: #asPoint )  size = 4).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point>> #roundDownTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundUpTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #roundTo:) asRingDefinition).
	self assert: (( Point allLocalCallsOn: #asPoint )  includes: (Point >> #truncateTo: ) asRingDefinition).
	
]

{ #category : #tests }
BehaviorTest >> testAllMethods [
	| allMethods nonOverridenMethods |
	allMethods := IdentitySet new
		addAll: Object allMethods;
		yourself.
	self assert: (allMethods includesAll: Object methods).	"We fetch all compiled methods that are not redefined in Object"
	nonOverridenMethods := OrderedCollection new.
	ProtoObject
		selectorsAndMethodsDo: [ :sel :method | 
			(Object includesSelector: sel)
				ifFalse: [ nonOverridenMethods add: method ] ].
	self assert: (allMethods includesAll: nonOverridenMethods)
]

{ #category : #tests }
BehaviorTest >> testAllReferencesTo [
	| result |
	result := SystemNavigation new allReferencesTo: Point binding.
	result do: [ :each | self assert: (each compiledMethod hasLiteral: Point binding) ].
	self assert: (result anySatisfy: [ :each | each actualClass = self class and: [ each selector = #testAllReferencesTo ] ]).
		
	result := SystemNavigation new allReferencesTo: #printOn:.
	result do: [ :each | self assert: (each compiledMethod hasLiteralThorough: #printOn:) ].
	self assert: (result anySatisfy: [ :each | each actualClass = self class and: [ each selector = #testAllReferencesTo ] ]).
	
	result := SystemNavigation new allReferencesTo: #+.
	result do: [ :each | self assert: ((each compiledMethod sendsSelector: #+) or: [ each compiledMethod hasLiteralThorough: #+ ]) ].
	self assert: (result anySatisfy: [ :each | each actualClass = self class and: [ each selector = #testAllReferencesTo ] ])
]

{ #category : #tests }
BehaviorTest >> testAllSelectors [
	self assert: ProtoObject allSelectors asSet = ProtoObject selectors asSet.
	self assert: Object allSelectors asSet = (Object selectors asSet union: ProtoObject selectors).
	self assert: (Object allSelectorsBelow: ProtoObject) asSet = (Object selectors) asSet.
]

{ #category : #tests }
BehaviorTest >> testAllSelectorsAbove [
		
	|sels |
	sels := Date allSelectorsAbove.
	self deny: (sels includes: #mmddyyyy). 
	self deny: (sels includes: #weekday).
	self assert: (sels includes: #at:).
	self assert: (sels includes: #cannotInterpret: )
	
]

{ #category : #tests }
BehaviorTest >> testAllSelectorsAboveUntil [
		
	|sels |
	sels := Date allSelectorsAboveUntil: Object..
	self deny: (sels includes: #mmddyyyy). 
	self deny: (sels includes: #weekday).
	self assert: (sels includes: #at:).
	self deny: (sels includes: #cannotInterpret: )
	
]

{ #category : #tests }
BehaviorTest >> testBehaviornewnewShouldNotCrash [

	Behavior new new.
	"still not working correctly but at least does not crash the image"
	
]

{ #category : #tests }
BehaviorTest >> testBinding [
	self assert: Object binding value = Object.
	self assert: Object binding key = #Object.
	
	self assert: Object class binding value = Object class.
	
	"returns nil for Metaclasses... like Encoder>>#associationFor:"
	
	self assert: Object class binding key isNil.
]

{ #category : #tests }
BehaviorTest >> testHasAbstractMethods [
	self deny: Object hasAbstractMethods.
	self deny: Object class hasAbstractMethods.
		
	"Behavior has abstract methods, for example hasTraitComposition, basicLocalSelectors:"
	self assert: Behavior hasAbstractMethods.
	self deny: Behavior class hasAbstractMethods.
	
	
	self assert: ObjectLayout hasAbstractMethods.
	"ObjectLayout defines methods because its class side contains abstract methods"
	self assert: ObjectLayout class hasAbstractMethods.
	
	
]

{ #category : #tests }
BehaviorTest >> testIncludesMethod [
	self assert: (Object includesMethod: Object>>#halt).
	self deny: (Class includesMethod: Object>>#halt).

	self assert: (Point includesMethod: Point>>#x).
	self deny: (LookupKey includesMethod: Point>>#x).
]

{ #category : #tests }
BehaviorTest >> testIsAbstract [

	self assert: Behavior isAbstract.	
	self deny: Behavior class isAbstract.
	self assert: ClassDescription isAbstract.
	self deny: ClassDescription class isAbstract.
	
	self deny: Class isAbstract.
	self deny: Class class isAbstract.
	self deny: Object isAbstract.
	self deny: Object class isAbstract.
]

{ #category : #tests }
BehaviorTest >> testIsRootInEnvironment [
	self assert: ProtoObject isRootInEnvironment.
	self deny: Object isRootInEnvironment
]

{ #category : #tests }
BehaviorTest >> testIsUsed [
	self assert: Object isUsed.
	self assert: Object class isUsed.

]

{ #category : #'tests - queries' }
BehaviorTest >> testMethodsAccessingSlot [
	| numberViaSlot numberViaIVar |
	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].
	
	numberViaSlot := (Point methodsAccessingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAccess: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
]

{ #category : #'tests - queries' }
BehaviorTest >> testMethodsReadingSlot [
	| numberViaSlot numberViaIVar |
	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].
	
	numberViaSlot := (Point methodsReadingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsRead: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
]

{ #category : #'tests - queries' }
BehaviorTest >> testMethodsWritingSlot [
	| numberViaSlot numberViaIVar |
	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].

	numberViaSlot := (Point methodsWritingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAssign: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
]

{ #category : #metrics }
BehaviorTest >> testNumberOfInstanceVariables [
	self assert: Object numberOfInstanceVariables = 0.
	self assert: Point numberOfInstanceVariables = 2.
	self assert: Metaclass numberOfInstanceVariables = 3
]

{ #category : #'tests - properties' }
BehaviorTest >> testPropertyValueAtPut [
	| testValue |
	testValue := Date today.
	self class propertyAt: #testKeySelector put: testValue.
	self
		assert: (self class propertyAt: #testKeySelector)
		equals: testValue.
	self class removeProperty: #testKeySelector
]

{ #category : #'tests - properties' }
BehaviorTest >> testRemoveProperty [
	self class propertyAt: #testKeySelector put: 1.
	self class removeProperty: #testKeySelector.
	self assert: (self class propertyAt: #testKeySelector) equals: nil
]

{ #category : #'tests - testing method dictionary' }
BehaviorTest >> testWhichSelectorsAccess [
	self assert: ((Point whichSelectorsAccess: 'x') includes: #x).
	self deny:  ((Point whichSelectorsAccess: 'y') includes: #x).
]

{ #category : #tests }
BehaviorTest >> testallSuperclassesIncluding [
		
	|cls |
	cls := ArrayedCollection allSuperclassesIncluding: Collection.
	self deny: (cls includes: ArrayedCollection).
	self deny: (cls includes: Object). 
	self assert: (cls includes: Collection). 
	self assert: (cls includes: SequenceableCollection). 
]
