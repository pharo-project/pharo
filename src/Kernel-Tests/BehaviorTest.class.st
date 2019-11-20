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
BehaviorTest >> testAllSelectors [
	self assert: ProtoObject allSelectors asSet equals: ProtoObject selectors asSet.
	self assert: Object allSelectors asSet equals: (Object selectors asSet union: ProtoObject selectors).
	self assert: (Object allSelectorsBelow: ProtoObject) asSet equals: Object selectors asSet
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
	sels := Date allSelectorsAboveUntil: Object.
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
	self assert: Object binding value equals: Object.
	self assert: Object binding key equals: #Object.

	self assert: Object class binding value equals: Object class.

	"returns nil for Metaclasses... like Encoder>>#associationFor:"

	self assert: Object class binding key isNil
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

{ #category : #metrics }
BehaviorTest >> testInstSize [
	self assert: Object instSize equals: 0.
	self assert: Point instSize equals: 2.
	self assert: Metaclass instSize equals: 6
]

{ #category : #tests }
BehaviorTest >> testIsAbstract [

	self deny: Behavior isAbstract.	
	self deny: Behavior class isAbstract.
	self deny: ClassDescription isAbstract.
	self deny: ClassDescription class isAbstract.
	
	self deny: Class isAbstract.
	self deny: Class class isAbstract.
	self deny: Object isAbstract.
	self deny: Object class isAbstract.
]

{ #category : #tests }
BehaviorTest >> testIsReferenced [
	self assert: Object isReferenced.
	self deny: Object class isReferenced
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
	(Point >> #x) hasSourceCode ifFalse: [ ^ self ].

	numberViaSlot := (Point methodsAccessingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAccess: 'x') size.
	self assert: numberViaSlot equals: numberViaIVar
]

{ #category : #'tests - queries' }
BehaviorTest >> testMethodsReadingSlot [
	| numberViaSlot numberViaIVar |
	"Check the source code availability to do not fail on images without sources"
	(Point >> #x) hasSourceCode ifFalse: [ ^ self ].

	numberViaSlot := (Point methodsReadingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsRead: 'x') size.
	self assert: numberViaSlot equals: numberViaIVar
]

{ #category : #'tests - queries' }
BehaviorTest >> testMethodsWritingSlot [
	| numberViaSlot numberViaIVar |
	"Check the source code availability to do not fail on images without sources"
	(Point >> #x) hasSourceCode ifFalse: [ ^ self ].

	numberViaSlot := (Point methodsWritingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsStoreInto: 'x') size.
	self assert: numberViaSlot equals: numberViaIVar
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

{ #category : #'tests - queries' }
BehaviorTest >> testallMethodsAccessingSlot [
	| methods |	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].


	methods := LookupKey allMethodsAccessingSlot: (LookupKey slotNamed: #key).
	self assert: (methods includes: (Association >>#key:value:))

]

{ #category : #'tests - queries' }
BehaviorTest >> testallMethodsReadingSlot [
	| methods |	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].

	methods := LookupKey allMethodsReadingSlot: (LookupKey slotNamed: #key).
	self assert: (methods includes: (Ephemeron >>#mourn))

]

{ #category : #'tests - queries' }
BehaviorTest >> testallMethodsWritingSlot [
	| methods |	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].


	methods := LookupKey allMethodsWritingSlot: (LookupKey slotNamed: #key).
	self assert: (methods includes: (Association >>#key:value:))

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

{ #category : #tests }
BehaviorTest >> testsourceCodeTemplateFor [
	"check for distictive source code templates for class-side and instance-side"
	self assert: ((Object sourceCodeTemplate) includesSubstring: 'instance-side message'). 
	self assert: ((Object class sourceCodeTemplate) includesSubstring: 'class-side message') 
]

{ #category : #'tests - queries' }
BehaviorTest >> testthoroughWhichMethodsReferTo [
	| array |
	array := #(thisIsOnlyHereIntestthoroughWhichMethodsReferTo).
	"normal case"
	self assert: (Point thoroughWhichMethodsReferTo: #x) notEmpty.
	"we understand send bytecodes for special selectors"
	self assert: (Point thoroughWhichMethodsReferTo: #+) notEmpty.
	"we dive into literal arrays"
	self assert: (self class thoroughWhichMethodsReferTo: array first) notEmpty.
	"and we are false for non existing symbols"
	self assert: (self class thoroughWhichMethodsReferTo: ('this', 'doesNotExist') asSymbol) isEmpty.
]
