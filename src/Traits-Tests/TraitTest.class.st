"
Unit test for trait handling
"
Class {
	#name : 'TraitTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitTest >> createT1 [
	| t1 |
	t1 := self newTrait: #T1 with: #(a b c).
	t1
		compile:
			'setValues
		a := 1.
		b := 2.
		c := 3.
	'.
	t1
		compile:
			'getValues
		^ a + b + c
	'.
	^ t1
]

{ #category : 'tests' }
TraitTest >> createT2 [
	| t2 |
	t2 := self newTrait: #T2 with: #(d e f).
	t2
		compile:
			'setValues2
		d := 1.
		e := 2.
		f := 3.
	'.
	t2
		compile:
			'getValues2
		^ d + e + f + self getValues
	'.
	^ t2
]

{ #category : 'tests' }
TraitTest >> createT3 [
	| t3 |

	"This is a trait with a method with a pragma"
	t3 := self newTrait: #T3 with: #().
	t3
		compile:
			'aMethod
			<aPragma>

			^ 42
				'.

	t3 class
		compile:
			'aClassMethod
			<aPragma>

			^ 42
				'.

	^ t3
]

{ #category : 'tests' }
TraitTest >> testAddingATraitToAClassWithSubclasses [
	"This is a regression test when adding a trait to a class without traits and with subclasses was not possible."

	| c1 c2 t1 |
	c1 := self newClass: #C1.
	c2 := self newClass: #C2 superclass: c1 traits: {  }.
	t1 := self newTrait: #T1.

	self shouldnt: [ self class classInstaller update: c1 to: [ :aBuilder | aBuilder traits: t1 ] ] raise: Error.

	self assert: (c1 includesTrait: t1).
	self deny: (c2 includesTrait: t1)
]

{ #category : 'tests' }
TraitTest >> testClassHavingAnInstanceVariableUsersDifferenThanUsers [

	| t1 aClass |
	t1 := self newTrait: #T1 with: #( users ).

	aClass := self newClass: #C1 superclass: Object traits: {  }.
	aClass class setTraitComposition: { t1 }.

	self assert: (aClass class allSlots anySatisfy: [ :e | e name = #users ]).
	self assert: (aClass class slotNamed: #users) definingClass equals: t1
]

{ #category : 'tests' }
TraitTest >> testClassTraitThatHasAPragmaHasCorrectTraitSourceAfterRecompile [

	| t3 aClass |
	t3 := self createT3.

	aClass := self newClass: #C1 superclass: Object traits: { t3 }.

	self assert: (aClass class >> #aClassMethod) traitSource equals: t3 class asTraitComposition.

	(aClass class >> #aClassMethod) recompile.

	self assert: (aClass class >> #aClassMethod) traitSource equals: t3 class asTraitComposition
]

{ #category : 'tests' }
TraitTest >> testClassTraits [
	| t1 t2 aClass |
	<ignoreNotImplementedSelectors: #(otherSelector otherSelector2 anAlias aSelector)>
	t1 := self newTrait: #T1 with: #().
	t2 := self newTrait: #T2 with: #().

	t1 classTrait compile: 'aSelector ^ 21'.
	t1 classTrait compile: 'otherSelector ^ 42'.

	t2 classTrait compile: 'otherSelector2 ^ 42'.

	aClass := self newClass: #C1 superclass: Object traits: {}.
	aClass class setTraitComposition: t1 classSide - { #aSelector }.

	self assert: aClass otherSelector equals: 42.

	aClass := self newClass: #C1 superclass: Object traits: {}.
	aClass class setTraitComposition: t1 classSide + t2 classSide.

	self assert: aClass otherSelector equals: 42.
	self assert: aClass otherSelector2 equals: 42.
	self assert: aClass aSelector equals: 21.


	aClass := self newClass: #C1 superclass: Object traits: {}.
	aClass class setTraitComposition: t1 classSide @ {#anAlias -> #aSelector}.

	self assert: aClass anAlias equals: 21.
	self assert: aClass aSelector equals: 21
]

{ #category : 'tests' }
TraitTest >> testClassUsesTrait [
	| t1 superclass subclass |
	t1 := self newTrait: #T1 with: {}.
	superclass := self newClass: #Superclass with:#() traits: t1.
	subclass := self newClass: #Subclass superclass: superclass traits: {}.

	self assert: (superclass usesTrait: t1).
	self assert: (superclass usesTraitLocally: t1).
	self assert: (subclass usesTrait: t1).
	self assert: (subclass usesTraitLocally: t1) not
]

{ #category : 'tests' }
TraitTest >> testClassUsingTraitsDoesNotHaveUsers [
	| t1 aClass |

	t1 := self newTrait: #T1 with: #().

	aClass := self newClass: #C1 superclass: Object traits: {t1}.

	self assert: (aClass class allSlots noneSatisfy: [:e | e name = #users])
]

{ #category : 'tests' }
TraitTest >> testComposedBy [
	"tests the #isComposedBy: aTrait method"

	self assert: (Trait3 isComposedBy: Trait2).
	self deny: (Trait2 isComposedBy: Trait3).
	self deny: (Trait3 isComposedBy: Object)
]

{ #category : 'tests' }
TraitTest >> testDefinedMethods [

	[
	Trait1 compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.
	Trait3 compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.
	MOPTestClassC compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.
	
	"Test local methods of a trait standalone"
	self assertCollection: Trait1 localMethods hasSameElements: { (Trait1 >> #c1). (Trait1 >> #c). (Trait1 >> #extensionMethod) }.
	self assertCollection: Trait1 definedMethods hasSameElements: { (Trait1 >> #c1). (Trait1 >> #c) }.
	
	"Test local methods of a trait using a trait"
	self assertCollection: Trait3 localMethods hasSameElements: { (Trait3 >> #c3). (Trait3 >> #c). (Trait3 >> #extensionMethod) }.
	self assertCollection: Trait3 definedMethods hasSameElements: { (Trait3 >> #c3). (Trait3 >> #c) }.
	
	"Test local methods of a class using a trait"
	self assertCollection: MOPTestClassC localMethods hasSameElements: { (MOPTestClassC >> #c).  (MOPTestClassC >> #extensionMethod)}.
	self assertCollection: MOPTestClassC definedMethods hasSameElements: { (MOPTestClassC >> #c) } ] ensure: [ self packageOrganizer removePackage: 'AGeneratedPackageForTest' ]
	
]

{ #category : 'tests' }
TraitTest >> testDefinedSelectors [

	[
	Trait1 compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.
	Trait3 compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.
	MOPTestClassC compile: 'extensionMethod ' classified: '*AGeneratedPackageForTest'.

	"Test local selectors of a trait standalone"
	self assertCollection: Trait1 localSelectors hasSameElements: #( #c1 #c #extensionMethod ).
	self assertCollection: Trait1 definedSelectors hasSameElements: #( #c1 #c ).

	"Test local selectors of a trait using a trait"
	self assertCollection: Trait3 localSelectors hasSameElements: #( #c3 #c #extensionMethod ).
	self assertCollection: Trait3 definedSelectors hasSameElements: #( #c3 #c ).

	"Test local selectors of a class using a trait"
	self assertCollection: MOPTestClassC localSelectors hasSameElements: #( #c #extensionMethod ).
	self assertCollection: MOPTestClassC definedSelectors hasSameElements: #( #c ) ] ensure: [ self packageOrganizer removePackage: 'AGeneratedPackageForTest' ]
]

{ #category : 'tests - empty' }
TraitTest >> testEmptyCompositionManagesTEmpty [

	| t1 |
	t1 := self newTrait: #T1 traits: TEmpty.

	self assert: t1 hasEmptyComposition
]

{ #category : 'tests' }
TraitTest >> testErrorClassCreation [

	| trait aSubclass aClass |
	trait := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #TMyTrait;
			         package: self packageNameForTests;
			         beTrait ].

	aClass := self class classInstaller make: [ :aClassBuilder |
		          aClassBuilder
			          name: #AClass;
			          superclass: nil;
			          package: self packageNameForTests ].

	"----------------"
	aSubclass := self class classInstaller make: [ :aClassBuilder |
		             aClassBuilder
			             name: #AClass2;
			             traitComposition: trait;
			             superclass: aClass;
			             package: self packageNameForTests ].

	"----------------"

	"Change the superclass of AClass"
	aClass := self class classInstaller make: [ :aClassBuilder |
		          aClassBuilder
			          name: #AClass;
			          superclass: Object;
			          package: self packageNameForTests ].

	self assert: trait traitUsers asArray equals: { aSubclass }.
	self assert: aSubclass traits asArray equals: { trait }
]

{ #category : 'tests' }
TraitTest >> testForbidInstantiation [

	| trait |
	trait := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #TMyTrait;
			         package: self packageNameForTests;
			         beTrait ].

	self should: [ trait basicNew ] raise: Error
]

{ #category : 'tests' }
TraitTest >> testIndirectSequence [
	| t1 t2 t3 c1 obj |
	<ignoreNotImplementedSelectors: #(setValues setValues2 getValues getValues2)>

	t1 := self createT1.
	t2 := self createT2.
	t3 := self newTrait: #T3 traits: t1 + t2.

	c1 := self newClass: #C1 with: #(g h) traits: t3.

	obj := c1 new.
	obj setValues.
	obj setValues2.

	self assert: (TraitedClass basicUsers includesAll:  { t1 class. t2 class. t3 class. c1 class}).

	self assert: obj getValues equals: 6.
	self assert: obj getValues2 equals: 12
]

{ #category : 'tests' }
TraitTest >> testIsUsed [

	self assert: Trait1 isUsed.
	self assert: Trait1 classTrait isUsed
]

{ #category : 'tests' }
TraitTest >> testLocalMethodWithSameCodeInTrait [
	"Test whether there are no unintentional overridden traits methods"

	SystemNavigation new
		allBehaviorsDo: [ :each |
			each hasTraitComposition
				ifTrue: [ each
						selectorsDo: [ :selector |
							(each includesLocalSelector: selector)
								ifTrue: [ (each traitComposition traitDefining: selector ifNone: [ nil ])
										ifNotNil: [ :trait | (trait selectors includes: selector) ifTrue: [ self deny: (trait compiledMethodAt: selector) equals: each >> selector ] ] ] ] ] ]
]

{ #category : 'tests' }
TraitTest >> testLocalMethods [
	
	"Test local methods of a trait standalone"
	self assertCollection: Trait1 localMethods hasSameElements: { (Trait1 >> #c1). (Trait1 >> #c) }.
	
	"Test local methods of a trait using a trait"
	self assertCollection: Trait3 localMethods hasSameElements: { (Trait3 >> #c3). (Trait3 >> #c) }.
	
	"Test local methods of a class using a trait"
	self assertCollection: MOPTestClassC localMethods hasSameElements: { (MOPTestClassC >> #c) }
]

{ #category : 'tests' }
TraitTest >> testLocalSelectors [

	"Test local selectors of a trait standalone"
	self assertCollection: Trait1 localSelectors hasSameElements: #( #c1 #c ).

	"Test local selectors of a trait using a trait"
	self assertCollection: Trait3 localSelectors hasSameElements: #( #c3 #c ).

	"Test local selectors of a class using a trait"
	self assertCollection: MOPTestClassC localSelectors hasSameElements: #( #c )
]

{ #category : 'tests' }
TraitTest >> testMethodsAddedInMetaclass [
	| t1 c1 |

	t1 := self createT1.
	c1 := self newClass: #C1 with: #(g h) traits: t1.

	self assertCollection: c1 class selectors sorted equals: TraitedClass selectors sorted
]

{ #category : 'tests' }
TraitTest >> testMethodsAddedInMetaclassNotPresentInSubclasses [

	| t1 c1 c2 |
	t1 := self createT1.
	c1 := self newClass: #C1 with: #( g h ) traits: t1.
	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	self assertCollection: c2 class localSelectors sorted equals: #(  )
]

{ #category : 'tests' }
TraitTest >> testMethodsAddedInMetaclassPresentInSubclassesAfterChangingSuperclass [
	| t1 c1 c2 t2 |

	t1 := self createT1.
	t2 := self createT2.

	c1 := self newClass: #C1 with: #(g h) traits: t1.
	c2 := self newClass: #C2 superclass: c1 traits: {t2}.

	self assertCollection: c2 class localSelectors sorted equals: #().
	self assert: (TraitedClass selectors allSatisfy: [ :selector | (c2 class lookupSelector: selector) isNotNil ])
	
]

{ #category : 'tests' }
TraitTest >> testOrigin [

	| tr1 tr2 tr3 tr23 |
	tr1 := self newTrait: #TTT1 traits: {  }.
	tr2 := self newTrait: #TTT2 traits: { tr1 }.
	tr3 := self newTrait: #TTT3 traits: { tr1 }.
	tr23 := self newTrait: #TTT23 traits: { (tr3 + tr2) }.

	tr1 compile: 'foo ^ 4'.
	self assert: (tr1 >> #foo) origin identicalTo: tr1.
	self assert: (tr2 >> #foo) origin identicalTo: tr1.
	self assert: (tr3 >> #foo) origin identicalTo: tr1.

	"-----------"
	"For TR2"

	self assert: (tr2 traitComposition traitDefining: #foo) innerClass identicalTo: tr1.
	self assert: (tr2 >> #foo) origin identicalTo: tr1.
	"-----------"

	"-----------"
	"For TR23"

	self assert: (tr23 traitComposition traitDefining: #foo) innerClass identicalTo: tr1.
	self assert: (tr23 >> #foo) origin identicalTo: tr1
	"----------"
]

{ #category : 'tests' }
TraitTest >> testOriginWithRequiredMethod [

	| tr1 c1 |
	tr1 := self newTrait: #TTT1 traits: {  }.

	tr1 compile: 'foo ^ self explicitRequirement'.
	self assert: (tr1 >> #foo) origin identicalTo: tr1.

	c1 := self newClass: #CTT1 superclass: Object traits: tr1.
	self assert: (c1 >> #foo) origin identicalTo: tr1
]

{ #category : 'tests' }
TraitTest >> testOriginWithRequiredMethodInTraitChain [
	"check that the origin of a explicit requirement is the trait defining this method, even if we check
this from another *trait* using the first trait. Tr2 uses Tr1, Tr1 defines a explicit requirement #foo, the
origin of Tr2>>foo is Tr1"

	| tr1 tr2 |
	tr1 := self newTrait: #TTT1 traits: {  }.
	tr1 compile: 'foo ^ self explicitRequirement'.
	self assert: (tr1 >> #foo) origin identicalTo: tr1.
	tr2 := self newTrait: #TTT2 traits: { tr1 }.
	self assert: (tr2 >> #foo) origin identicalTo: tr1
]

{ #category : 'tests' }
TraitTest >> testPackageIsUpdatedInClassSide [

	| c1 t1 |
	t1 := self newTrait: #T1.
	t1 class compile: 'msg ^ 1'.

	c1 := self newClass: #C1 superclass: Object traits: t1.
	c1 class compile: 'msg ^ 12'.

	self assert: self packageNameForTests asPackage methods size equals: 2.
	(c1 class >> #msg) removeFromSystem.
	self assert: self packageNameForTests asPackage methods size equals: 1
]

{ #category : 'tests' }
TraitTest >> testPackageIsUpdatedInInstanceSide [

	| c1 t1 |
	t1 := self newTrait: #T1.
	t1 compile: 'msg ^ 1'.

	c1 := self newClass: #C1 superclass: Object traits: t1.
	c1 compile: 'msg ^ 12'.

	self assert: (c1 >> #msg) origin equals: c1.

	self assert: self packageNameForTests asPackage methods size equals: 2.
	(c1 >> #msg) removeFromSystem.
	self assert: self packageNameForTests asPackage methods size equals: 1
]

{ #category : 'tests' }
TraitTest >> testRecompilingTraitClassMethodRecompilesTheMethodInTheUsers [

	| trait class priorTraitMethod priorClassMethod |
	trait := self newTrait: #TTraitForTest.

	class := self newClass: #ClassUsingTTraitForTest traits: trait.

	trait class compile: 'test
    ^ #test'.


	priorTraitMethod := trait class >> #test.
	priorClassMethod := class class >> #test.

	priorTraitMethod recompile.

	self deny: trait class >> #test identicalTo: priorTraitMethod.
	self deny: class class >> #test identicalTo: priorClassMethod
]

{ #category : 'tests' }
TraitTest >> testRecompilingTraitMethodRecompilesTheMethodInTheUsers [

	| trait class priorTraitMethod priorClassMethod |
	trait := self newTrait: #TTraitForTest.

	class := self newClass: #ClassUsingTTraitForTest traits: trait.
	
	trait compile: 'test
    ^ #test'.


	priorTraitMethod := trait >> #test.
	priorClassMethod := class >> #test.

	priorTraitMethod recompile.

	self deny: trait >> #test identicalTo: priorTraitMethod.
	self deny: class >> #test identicalTo: priorClassMethod
]

{ #category : 'tests' }
TraitTest >> testRedefiningAClassAsTraitShouldRaiseError [

	self newClass: #C1.
	self should: [ self newTrait: #C1 ] raise: Error
]

{ #category : 'tests' }
TraitTest >> testRedefiningATraitAsAClassShouldRaiseError [

	self newTrait: #C1 with: #().
	self should: [ 	self newClass: #C1 ] raise: Error
]

{ #category : 'tests' }
TraitTest >> testRemakingATraitUsedByAnAnonymousClassKeepItAnonymous [
	"Regression test were an anonymous class became non anonymous if we updated a trait used by it."

	| t1 aClass |
	t1 := self newTrait: #T1 with: #(  ).

	aClass := Smalltalk anonymousClassInstaller make: [ :builder | builder traitComposition: t1 ].

	self assert: aClass isAnonymous.

	t1 := self newTrait: #T1 with: #( aSlot ).

	self assert: aClass isAnonymous
]

{ #category : 'tests' }
TraitTest >> testRemoveFromSystem [

	| aClass aTrait |
	aTrait := self newTrait: #T1.
	aClass := self newClass: #AClassForTests traits: aTrait.
	aTrait removeFromSystem: false.
	self deny: aClass hasTraitComposition.
	self assert: aTrait isObsolete
]

{ #category : 'tests' }
TraitTest >> testRemovingTraitsDoesNotModifiyTraitedSubclasses [

	| t1 t2 c2 c1 |
	t1 := self newTrait: #T1.
	t2 := self newTrait: #T2.
	c1 := self newClass: #C1 traits: t1.
	c2 := self newClass: #C2 superclass: c1 traits: t2.

	self assert: c1 hasTraitComposition.
	self assert: (c1 traits includes: t1).
	self assert: c2 hasTraitComposition.
	self assert: (c2 traits includes: t2).
	self assert: (t2 users includes: c2).

	c1 := self newClass: #C1 traits: {  }.

	self deny: c1 hasTraitComposition.
	self deny: (c1 traits includes: t1).
	self assert: c2 hasTraitComposition.
	self assert: (c2 traits includes: t2).
	self assert: (t2 users includes: c2)
]

{ #category : 'tests' }
TraitTest >> testRemovingTraitsRemoveTraitedClassMethods [
	| t1 t2 c1 |

	t1 := self createT1.
	t2 := self createT2.
	c1 := self newClass: #C1 with: #(g h) traits: t1 + t2.

	self assert: (c1 class includesSelector: #traits).

	c1 := self newClass: #C1 with: #(g h) traits: {}.

	self deny: (c1 class includesSelector: #traits)
]

{ #category : 'tests' }
TraitTest >> testRemovingTraitsRemoveTraitedClassMethodsWithSubclasses [
	| t1 t2 c2 c1 c3 |

	t1 := self createT1.
	t2 := self createT2.
	c1 := self newClass: #C1 with: #(g h) traits: t1.
	c2 := self newClass: #C2 superclass: c1 traits: t2.
	c3 := self newClass: #C3 superclass: c1 traits: {}.

	self assert: (c1 class includesSelector: #traits).
	self deny: (c1 class includesLocalSelector: #traits).
	self assert: (c2 class lookupSelector: #traits) isNotNil.
	self deny: (c2 class includesLocalSelector: #traits).
	self assert: (c3 class lookupSelector: #traits) isNotNil.
	self deny: (c3 class includesLocalSelector: #traits).

	c1 := self newClass: #C1 with: #(g h) traits: {}.

	"Now c1 has no more traits, c2 remains unchanged"
	self deny: (c1 class includesSelector: #traits).
	self assert: (c2 class includesSelector: #traits).
	self deny: (c2 class includesLocalSelector: #traits).
	self assert: (c2 class includesSelector: #traits).
	self deny: (c2 class includesLocalSelector: #traits).
]

{ #category : 'tests' }
TraitTest >> testRemovingTraitsUpdatesCategories [

	| t1 t2 c1 |
	t1 := self createT1.
	t2 := self createT2.
	c1 := self newClass: #C1 with: #(g h) traits: t1 + t2.
	c1 := self newClass: #C1 with: #(g h) traits: {  }.

	c1 selectors do: [ :selector | self assert: (c1 includesSelector: selector) ].
	c1 class selectors do: [ :selector | self assert: (c1 class includesSelector: selector) ]
]

{ #category : 'tests' }
TraitTest >> testSelectorsWithExplicitOrigin [
	"Obtain the subset of selectors that are defined either locally or inherited from traits. But, exclude selectors of methods from implicit traits such as TraitedClass"
	| t1 c1 |

	t1 := self newTrait: #T1 with: #().
	t1 compile: 'instanceSideMethod'.
	t1 class compile: 'classSideMethod'.
	c1 := self newClass: #C1 traits: t1.
	self assertCollection: c1 selectorsWithExplicitOrigin hasSameElements: #(instanceSideMethod).
	self assertCollection: c1 class selectorsWithExplicitOrigin hasSameElements: #(classSideMethod)
]

{ #category : 'tests' }
TraitTest >> testSelectorsWithExplicitOriginNoTrait [
	"Obtain the subset of selectors that are defined either locally or inherited from traits. But, exclude selectors of methods from implicit traits such as TraitedClass"
	| c1 |
	c1 := self newClass: #C1.
	c1 compile: 'instanceSideMethod'.
	c1 class compile: 'classSideMethod'.
	self assertCollection: c1 selectorsWithExplicitOrigin hasSameElements: #(instanceSideMethod).
	self assertCollection: c1 class selectorsWithExplicitOrigin hasSameElements: #(classSideMethod)
]

{ #category : 'tests' }
TraitTest >> testSequence [
	| t1 t2 c1 obj |
	<ignoreNotImplementedSelectors: #(setValues setValues2 getValues getValues2 )>

	t1 := self createT1.
	t2 := self createT2.
	c1 := self newClass: #C1 with: #(g h) traits: t1 + t2.

	obj := c1 new.
	obj setValues.
	obj setValues2.


	self assert: (TraitedClass basicUsers includesAll:  { t1 class. t2 class. c1 class}).

	self assert: obj getValues equals: 6.
	self assert: obj getValues2 equals: 12
]

{ #category : 'tests' }
TraitTest >> testSettingAClassInAClassTraitCompositionShouldRaiseAnError [

	| t1 c1 |
	t1 :=  self newTrait: #T1 with: #(a).
	c1 := self newClass: #C1.

	self should: [ t1 traitComposition: c1 ] raise: Error.
	self should: [ t1 classTrait traitComposition: c1 ] raise: Error.
]

{ #category : 'tests' }
TraitTest >> testSettingEmptyTraitCompositionDoesNotModifiyTraitedSubclasses [

	| t1 t2 c2 c1 |
	t1 := self createT1.
	t2 := self createT2.
	c1 := self newClass: #C1 with: #( g h ) traits: t1.
	c2 := self newClass: #C2 superclass: c1 traits: t2.

	c1 setTraitComposition: {  }.

	self assert: c2 hasTraitComposition.
	self assert: (c2 traits includes: t2).
	self assert: (t2 users includes: c2)
]

{ #category : 'tests' }
TraitTest >> testSettingEmptyTraitCompositionUpdatesMetaclass [

	| t1 t2 c2 c1 c3 |
	t1 := self createT1.
	t2 := self createT2.
	c1 := self newClass: #C1 with: #( g h ) traits: t1.
	c2 := self newClass: #C2 superclass: c1 traits: t2.
	c3 := self newClass: #C3 superclass: c1 traits: {  }.

	self assert: (c1 class includesSelector: #traits).
	self deny: (c2 class includesSelector: #traits).
	self deny: (c3 class includesSelector: #traits).

	c1 setTraitComposition: {  }.

	self deny: (c1 class includesSelector: #traits).
	self assert: (c2 class includesSelector: #traits).
	self deny: (c3 class includesSelector: #traits)
]

{ #category : 'tests' }
TraitTest >> testSlotsAreNotDuplicated [

	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #(a).
	t2 := self newTrait: #T2 traits: t1.
	c1 := self newClass: #C1 traits: t1 + t2.

	self assert: c1 traitComposition slots size equals: c1 traitComposition slots asSet size.
	self assert: c1 traitComposition slots size equals: 1
]

{ #category : 'tests' }
TraitTest >> testSubclasses [
	| t1 t2 |

	t1 := self createT1.
	t2 := self newTrait: #T2 with: #(aa bb) traits: t1.

	self deny: t1 hasSubclasses.
	self deny: t2 hasSubclasses.

	self assert: t1 subclasses isEmpty.
	self assert: t2 subclasses isEmpty.

	self deny: t1 hasSubclasses.
	self deny: t2 hasSubclasses.
]

{ #category : 'tests' }
TraitTest >> testTraitHaveUsersInstanceVariable [
	| t1 aClass |

	t1 := self newTrait: #T1 with: #(users).

	aClass := self newClass: #C1 superclass: Object traits: {t1}.

	self assert: (aClass allSlots anySatisfy: [:e | e name = #users]).
	self assert: (aClass slotNamed: #users) definingClass equals: t1
]

{ #category : 'tests' }
TraitTest >> testTraitRemoval [

	| aClass aTrait |
	aTrait := self newTrait: #T1.
	aClass := self newClass: #AClassForTests traits: aTrait.
	self assertEmpty: aClass localSelectors.
	aClass removeFromComposition: aTrait.
	self assertEmpty: aClass localSelectors.
	self assertEmpty: aClass selectors.
	self deny: aClass hasTraitComposition.
	self deny: (aTrait traitUsers includes: aClass)
]

{ #category : 'tests' }
TraitTest >> testTraitSource [

	self assert: (MOPTestClassC >> #c) traitSource isNil.
	self assert: (MOPTestClassC >> #c2) traitSource equals: Trait2 asTraitComposition
]

{ #category : 'tests' }
TraitTest >> testTraitSourceIsPersistedWithRecompilation [

	[
	Trait2 compile: 'traitMethod ^ 1' classified: '*GeneratedPackageForTest'.

	self assert: (Trait2 >> #traitMethod) traitSource isNil.
	self assert: (MOPTestClassC >> #traitMethod) traitSource equals: Trait2 asTraitComposition.

	(Trait2 >> #traitMethod) recompile.

	self assert: (Trait2 >> #traitMethod) traitSource isNil.
	self assert: (MOPTestClassC >> #traitMethod) traitSource equals: Trait2 asTraitComposition ] ensure: [
		self packageOrganizer removePackage: 'GeneratedPackageForTest' ]
]

{ #category : 'tests' }
TraitTest >> testTraitSourceIsPersistedWithRemovalOfMetalinks [

	[
	| metalink |
	Trait2 compile: 'traitMethod ^ 1' classified: '*GeneratedPackageForTest'.

	self assert: (Trait2 >> #traitMethod) traitSource isNil.
	self assert: (MOPTestClassC >> #traitMethod) traitSource equals: Trait2 asTraitComposition.

	metalink := MetaLink new.
	(MOPTestClassC >> #traitMethod) ast link: metalink.

	self assert: (Trait2 >> #traitMethod) traitSource isNil.
	self assert: (MOPTestClassC >> #traitMethod) traitSource equals: Trait2 asTraitComposition.

	metalink uninstall.

	self assert: (Trait2 >> #traitMethod) traitSource isNil.
	self assert: (MOPTestClassC >> #traitMethod) traitSource equals: Trait2 asTraitComposition ] ensure: [
		self packageOrganizer removePackage: 'GeneratedPackageForTest' ]
]

{ #category : 'tests' }
TraitTest >> testTraitThatHasAPragmaHasCorrectTraitSourceAfterRecompile [
	| t3 aClass |

	t3 := self createT3.

	aClass := self newClass: #C1 superclass: Object traits: {t3}.

	self assert: (aClass >> #aMethod) traitSource equals: t3 asTraitComposition.

	(aClass >> #aMethod) recompile.

	self assert: (aClass >> #aMethod) traitSource equals: t3 asTraitComposition
]

{ #category : 'tests' }
TraitTest >> testTraitUsingTraitsPreserveSourceCode [

	| t1 t2 source |
	t1 := self createT1.
	t2 := self newTrait: #T2 traits: t1.

	source := 'aMethod: aString
	^ aMethod'.
	t1 compile: source.

	self assert: (t1 >> #aMethod:) sourceCode equals: source.
	self assert: (t2 >> #aMethod:) sourceCode equals: source
]

{ #category : 'tests' }
TraitTest >> testTraitUsingTraitsPreserveSourceCodeOnClassSide [

	| t1 t2 source |
	t1 := self createT1.
	t2 := self newTrait: #T2 traits: t1.

	source := 'aMethod: aString
	^ aMethod'.
	t1 class compile: source.

	self assert: (t1 class >> #aMethod:) sourceCode equals: source.
	self assert: (t2 class >> #aMethod:) sourceCode equals: source
]

{ #category : 'tests' }
TraitTest >> testTraitsMethodClassSanity [

	(Smalltalk globals allTraits flatCollect: #traitUsers) asSet do: [ :trait |
		trait selectorsDo: [ :selector | self assert: (trait >> selector) methodClass identicalTo: trait ] ]
]

{ #category : 'tests' }
TraitTest >> testTraitsUsersSanity [
	"This documents bug http://code.google.com/p/pharo/issues/detail?id=443"

	Smalltalk allClassesAndTraits do: [ :each | self assert: (each traits allSatisfy: [ :t | t traitUsers includes: each ]) ].
	Smalltalk globals allTraits do: [ :each | self assert: (each traitUsers allSatisfy: [ :b | b traits includes: each ]) ]
]

{ #category : 'tests' }
TraitTest >> testUsingTraitInAnonymousSubClassAndRedefiningIt [
	| t1 aClass |

	t1 := self newTrait: #T1 with: #().

	aClass := Smalltalk anonymousClassInstaller make: [ :builder |
		builder superclass: Object.
		builder traitComposition: t1.
	].

	self deny: (Object subclasses includes: aClass).

	t1 := t1 classInstaller 
				update: t1
				to: [ :aBuilder |
		  						aBuilder
			  					slots: #(aSlot);
								 package: self packageNameForTests;
			  					beTrait ].
		
	self assert: (aClass hasSlotNamed: #aSlot).

	self deny: (Object subclasses includes: aClass)
]
