"
SUnit tests for Behavior
"
Class {
	#name : 'BehaviorTest',
	#superclass : 'ClassTestCase',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'coverage' }
BehaviorTest >> classToBeTested [

	^ Behavior
]

{ #category : 'tests' }
BehaviorTest >> sampleMessageWithFirstArgument: firstArgument "This is a comment intended to explain arg1"
   andInterleavedCommentBeforeSecondArgument: secondArgument [

	"This method is here to test a few utilities like formalParametersAt:"

	| thisIsAnUnusedTemp |
	thisIsAnUnusedTemp := self.
	^thisIsAnUnusedTemp
]

{ #category : 'tests' }
BehaviorTest >> testAllInstVarNames [

	self assert: Point allInstVarNames equals: #( x y ).
	"superclass ivars are first"
	self assert: Association allInstVarNames equals: #( key value ).
	"For now, allInstVarNames is implemented by returning all slot names. This has the be improved
	later: for ST80 compatibilty, we should have allInstVarNames just returning the indexed slots,
	while allInstanceVariableNames should return all"
	self assert:
		(SystemNavigation new allBehaviors allSatisfy: [ :behavior |
			 behavior allInstVarNames size = behavior allSlots size ])
]

{ #category : 'tests' }
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

{ #category : 'tests' }
BehaviorTest >> testAllSelectors [
	self assert: ProtoObject allSelectors asSet equals: ProtoObject selectors asSet.
	self assert: Object allSelectors asSet equals: (Object selectors asSet union: ProtoObject selectors).
	self assert: (Object allSelectorsBelow: ProtoObject) asSet equals: Object selectors asSet
]

{ #category : 'tests' }
BehaviorTest >> testAllSelectorsAbove [

	| selectors |
	selectors := self class allSelectorsAbove.
	self deny: (selectors includes: #testAllSelectorsAboveUntil).
	self assert: (selectors includes: #assert:).
	self assert: (selectors includes: #yourself)
]

{ #category : 'tests' }
BehaviorTest >> testAllSelectorsAboveUntil [

	| selectors |
	selectors := self class allSelectorsAboveUntil: TestCase.
	self deny: (selectors includes: #testAllSelectorsAboveUntil).
	self assert: (selectors includes: #run:).
	self deny: (selectors includes: #yourself)
]

{ #category : 'tests' }
BehaviorTest >> testBehaviornewnewShouldNotCrash [

	Behavior new new.
	"still not working correctly but at least does not crash the image"
]

{ #category : 'tests' }
BehaviorTest >> testBinding [
	self assert: Object binding value equals: Object.
	self assert: Object binding key equals: #Object.

	self assert: Object class binding value equals: Object class.

	"returns nil for Metaclasses... like Encoder>>#associationFor:"

	self assert: Object class binding key isNil
]

{ #category : 'tests' }
BehaviorTest >> testDefinedMethods [

	[
	ExampleForTest1 compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.
	self assertCollection: ExampleForTest1 localMethods hasSameElements: {
			(ExampleForTest1 >> #aSuperclassVariable).
			(ExampleForTest1 >> #aSuperclassVariable:).
			(ExampleForTest1 >> #extensionMethod) }.

	self assertCollection: ExampleForTest1 definedMethods hasSameElements: {
			(ExampleForTest1 >> #aSuperclassVariable).
			(ExampleForTest1 >> #aSuperclassVariable:) } ] ensure: [ self packageOrganizer removePackage: 'AGeneratedPackageForTest' ]
]

{ #category : 'tests' }
BehaviorTest >> testDefinedSelectors [

	[
	ExampleForTest1 compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.
	self assertCollection: ExampleForTest1 localSelectors hasSameElements: #( #aSuperclassVariable #aSuperclassVariable: #extensionMethod ).

	self assertCollection: ExampleForTest1 definedSelectors hasSameElements: #( #aSuperclassVariable #aSuperclassVariable: ) ] ensure: [
		self packageOrganizer removePackage: 'AGeneratedPackageForTest' ]
]

{ #category : 'tests - queries' }
BehaviorTest >> testDefinedVariables [
	self assert: Behavior new definedVariables isEmpty.
	self assert: Point definedVariables equals: Point instanceVariables.
	self assert: EmptyLayout class definedVariables first name equals: #instance.
	self assert: (SmalltalkImage definedVariables includes: (SmalltalkImage classVariableNamed: #CompilerClass))
]

{ #category : 'tests' }
BehaviorTest >> testHasAbstractMethods [
	self deny: Object hasAbstractMethods.
	self deny: Object class hasAbstractMethods.

	"Behavior has abstract methods, for example hasTraitComposition, basicLocalSelectors:"
	self assert: Behavior hasAbstractMethods.
	self deny: Behavior class hasAbstractMethods.


	self assert: ObjectLayout hasAbstractMethods.
	"ObjectLayout defines methods because its class side contains abstract methods"
	self assert: ObjectLayout class hasAbstractMethods
]

{ #category : 'tests - properties' }
BehaviorTest >> testHasProperty [

	self class propertyAt: #testKeySelector put: 42.
	self assert: (self class hasProperty: #testKeySelector).

	self class removeProperty: #testKeySelector.
	self deny: (self class hasProperty: #testKeySelector)
]

{ #category : 'tests - testing - class hierarchy' }
BehaviorTest >> testIncludesBehavior [
	self assert: (Object includesBehavior: Object).
	self assert: (Behavior includesBehavior: Object).
	self deny: (Object includesBehavior: Behavior).
	self deny: (Protocol includesBehavior: Behavior).
	self deny: (Behavior includesBehavior: Protocol).
	"it should work for metaclasses, too"
	self assert: (Metaclass class includesBehavior: Class).
	self assert: (Metaclass class includesBehavior: Behavior class)
]

{ #category : 'tests' }
BehaviorTest >> testIncludesMethod [
	self assert: (Object includesMethod: Object>>#halt).
	self deny: (Class includesMethod: Object>>#halt).

	self assert: (Point includesMethod: Point>>#x).
	self deny: (LookupKey includesMethod: Point>>#x)
]

{ #category : 'tests - testing - class hierarchy' }
BehaviorTest >> testInheritsFrom [
	self deny: (Object inheritsFrom: Object).
	self assert: (Behavior inheritsFrom: Object).
	self deny: (Object inheritsFrom: Behavior).
	self deny: (Protocol inheritsFrom: Behavior).
	self deny: (Behavior inheritsFrom: Protocol).
	"it should work for metaclasses, too"
	self assert: (Metaclass class inheritsFrom: Class).
	self assert: (Metaclass class inheritsFrom: Behavior class)
]

{ #category : 'metrics' }
BehaviorTest >> testInstSize [
	self assert: Object instSize equals: 0.
	self assert: Point instSize equals: 2.
	self assert: Metaclass instSize equals: 6
]

{ #category : 'tests' }
BehaviorTest >> testIsAbstract [

	self deny: Behavior isAbstract.
	self assert: ClassDescription isAbstract.
	
	self deny: Class isAbstract.
	self deny: Object isAbstract
]

{ #category : 'tests - testing - class hierarchy' }
BehaviorTest >> testIsInClassHierarchyOf [
	self assert: (Object isInClassHierarchyOf: Object).
	self assert: (Behavior isInClassHierarchyOf: Object).
	self assert: (Object isInClassHierarchyOf: Behavior).
	self deny: (Protocol isInClassHierarchyOf: Behavior).
	self deny: (Behavior isInClassHierarchyOf: Protocol).
	"it should work for metaclasses, too"
	self assert: (Metaclass class isInClassHierarchyOf: Class).
	self assert: (Metaclass class isInClassHierarchyOf: Behavior class)
]

{ #category : 'tests' }
BehaviorTest >> testIsReferenced [
	self assert: Object isReferenced.
	self deny: Object class isReferenced
]

{ #category : 'tests' }
BehaviorTest >> testIsRootInEnvironment [
	self assert: ProtoObject isRootInEnvironment.
	self deny: Object isRootInEnvironment
]

{ #category : 'tests' }
BehaviorTest >> testIsUsed [
	self assert: Object isUsed.
	self assert: Object class isUsed
]

{ #category : 'tests' }
BehaviorTest >> testLocalMethods [

	self assertCollection: ExampleForTest1 localMethods hasSameElements: {
			(ExampleForTest1 >> #aSuperclassVariable).
			(ExampleForTest1 >> #aSuperclassVariable:) }
]

{ #category : 'tests' }
BehaviorTest >> testLocalSelectors [

	self assertCollection: ExampleForTest1 localSelectors hasSameElements: #( #aSuperclassVariable #aSuperclassVariable: )
]

{ #category : 'tests - queries' }
BehaviorTest >> testMethodsAccessingSlot [
	| numberViaSlot numberViaIVar |
	"Check the source code availability to do not fail on images without sources"
	(Point >> #x) hasSourceCode ifFalse: [ ^ self ].

	numberViaSlot := (Point methodsAccessingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAccess: 'x') size.
	self assert: numberViaSlot equals: numberViaIVar
]

{ #category : 'tests - queries' }
BehaviorTest >> testMethodsReadingSlot [
	| numberViaSlot numberViaIVar |
	"Check the source code availability to do not fail on images without sources"
	(Point >> #x) hasSourceCode ifFalse: [ ^ self ].

	numberViaSlot := (Point methodsReadingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsRead: 'x') size.
	self assert: numberViaSlot equals: numberViaIVar
]

{ #category : 'tests - queries' }
BehaviorTest >> testMethodsWritingSlot [
	| numberViaSlot numberViaIVar |
	"Check the source code availability to do not fail on images without sources"
	(Point >> #x) hasSourceCode
		ifFalse: [ ^ self ].
	numberViaSlot := (Point methodsWritingSlot: (Point slotNamed: #x))
		size.
	numberViaIVar := (Point whichSelectorsWrite: 'x') size.
	self assert: numberViaSlot equals: numberViaIVar
]

{ #category : 'tests' }
BehaviorTest >> testNonObsoleteClass [
	"Does it work on not-obsolete classes?"
	self assert: Object nonObsoleteClass equals: Object.
	"The case for obsolete classes (obtaining the #originalName) is tested
	in ObsoleteTest>>#testClassObsolete"
]

{ #category : 'tests' }
BehaviorTest >> testOriginalName [
	"Does it work on not-obsolete classes?"
	self assert: Object originalName equals: #Object.
	"The case for obsolete classes is tested in ObsoleteTest>>#testClassObsolete"
]

{ #category : 'tests - properties' }
BehaviorTest >> testPropertyValueAtPut [

	self class propertyAt: #testKeySelector put: 42.
	self assert: (self class propertyAt: #testKeySelector) equals: 42.
	self class removeProperty: #testKeySelector
]

{ #category : 'tests - properties' }
BehaviorTest >> testPropertyValueAtPutChangeGetLatestPutValue [

	self class propertyAt: #testKeySelector put: 42.
	self assert: (self class propertyAt: #testKeySelector) equals: 42.
	self class propertyAt: #testKeySelector put: 46.
	self assert: (self class propertyAt: #testKeySelector) equals: 46.
	self class removeProperty: #testKeySelector
]

{ #category : 'tests - properties' }
BehaviorTest >> testRemoveProperty [
	self class propertyAt: #testKeySelector put: 1.
	self class removeProperty: #testKeySelector.
	self assert: (self class propertyAt: #testKeySelector) equals: nil
]

{ #category : 'tests - queries' }
BehaviorTest >> testThoroughWhichMethodsReferTo [
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
	"we can search for Literal Variable references, but it is faster to use the non-thorough version"
	self assert: (self class thoroughWhichSelectorsReferTo: Point binding) notEmpty
]

{ #category : 'tests - queries' }
BehaviorTest >> testThoroughWhichMethodsReferToSpecialIndex [
	| array |
	array := #(thisIsOnlyHereIntestthoroughWhichMethodsReferTo).
	"normal case"
	self assert: (Point thoroughWhichMethodsReferTo: #x specialIndex: ( Smalltalk specialSelectorIndexOrNil: #+)) notEmpty.
	"we understand send bytecodes for special selectors"
	self assert: (Point thoroughWhichMethodsReferTo: #+ specialIndex: ( Smalltalk specialSelectorIndexOrNil: #+)) notEmpty.
	"we dive into literal arrays"
	self assert: (self class thoroughWhichMethodsReferTo: array first specialIndex: nil) notEmpty.
	"and we are false for non existing symbols"
	self assert: (self class thoroughWhichMethodsReferTo: ('this', 'doesNotExist') asSymbol specialIndex: nil) isEmpty
]

{ #category : 'tests - queries' }
BehaviorTest >> testThoroughWhichSelectorsReferTo [
	| array |
	array := #(thisIsOnlyHereIntestthoroughWhichSelectorsReferTo).
	"normal case"
	self assert: (Point thoroughWhichSelectorsReferTo: #x) notEmpty.
	"we understand send bytecodes for special selectors"
	self assert: (Point thoroughWhichSelectorsReferTo: #+) notEmpty.
	"we dive into literal arrays"
	self assert: (self class thoroughWhichSelectorsReferTo: array first) notEmpty.
	"and we are false for non existing symbols"
	self assert: (self class thoroughWhichSelectorsReferTo: ('this', 'doesNotExist') asSymbol) isEmpty.
	"we can search for Literal Variable references, but it is faster to use the non-thorough version"
	self assert: (self class thoroughWhichSelectorsReferTo: Point binding) notEmpty
]

{ #category : 'tests - queries' }
BehaviorTest >> testallMethodsAccessingSlot [
	| methods |
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].


	methods := LookupKey allMethodsAccessingSlot: (LookupKey slotNamed: #key).
	self assert: (methods includes: (Association >>#key:value:))
]

{ #category : 'tests - queries' }
BehaviorTest >> testallMethodsReadingSlot [
	| methods |
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].

	methods := LookupKey allMethodsReadingSlot: (LookupKey slotNamed: #key).
	self assert: (methods includes: (LookupKey >>#=))
]

{ #category : 'tests - queries' }
BehaviorTest >> testallMethodsWritingSlot [
	| methods |
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].


	methods := LookupKey allMethodsWritingSlot: (LookupKey slotNamed: #key).
	self assert: (methods includes: (Association >>#key:value:))
]

{ #category : 'tests' }
BehaviorTest >> testallSuperclassesIncluding [

	|cls |
	cls := ArrayedCollection allSuperclassesIncluding: Collection.
	self deny: (cls includes: ArrayedCollection).
	self deny: (cls includes: Object).
	self assert: (cls includes: Collection).
	self assert: (cls includes: SequenceableCollection)
]

{ #category : 'tests' }
BehaviorTest >> testsourceCodeTemplateFor [
	"check for distictive source code templates for class-side and instance-side"
	self assert: ((Object sourceCodeTemplate) includesSubstring: 'instance-side method').
	self assert: ((Object class sourceCodeTemplate) includesSubstring: 'class-side method')
]
