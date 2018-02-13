Class {
	#name : #TraitTest,
	#superclass : #TraitsTestCase,
	#category : #'Tests-Traits'
}

{ #category : #utils }
TraitTest >> createClassUsing: aTrait [
	^ Object
		subclass: #AClassForTests
		uses: aTrait
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'AAA'
]

{ #category : #utils }
TraitTest >> createTrait [
	| trait |
	trait := Trait named: #ATraitForTests uses: {} category: 'AAA'.
	trait compile: 'm1 ^1' classified: 'test'.
	^ trait
]

{ #category : #testing }
TraitTest >> testAddAndRemoveMethodsFromSubtraits [
	| aC2 |
	aC2 := self c2 new.
	self assert: aC2 m51.
	self t5 removeSelector: #m51.
	self should: [ aC2 m51 ] raise: MessageNotUnderstood.
	self t1 compile: 'foo ^true'.
	self deny: aC2 foo.
	self t1 compile: 'm51 ^self'.
	aC2 m51.
	self assert: aC2 m51 == aC2
]

{ #category : #testing }
TraitTest >> testAddAndRemoveMethodsInClassOrTrait [
	| aC2 |
	aC2 := self c2 new.
	self assert: aC2 m51.
	self c2 compile: 'm51 ^123'.
	self assert: aC2 m51 = 123.
	self c2 removeSelector: #m51.
	aC2 m51.
	self assert: aC2 m51.
	self t4 removeSelector: #m11.
	self assert: (self t4 includesSelector: #m11)
]

{ #category : #testing }
TraitTest >> testAllClassVarNames [	
	
	self assert: self t1 allClassVarNames isEmpty
]

{ #category : #testing }
TraitTest >> testComposedBy [
	"tests the #isComposedBy: aTrait method"
	
	self assert: (Trait3 isComposedBy: Trait2).
	self deny: (Trait2 isComposedBy: Trait3).
	self deny: (Trait3 isComposedBy: Object).
	
]

{ #category : #testing }
TraitTest >> testCompositionCopy [
	| t6compositionCopyFirst c2compositionCopy |
	self assert: (self t1 + self t2) allTraits 
				= (self t1 + self t2) copyTraitExpression allTraits.
	self assert: (self t1 classTrait + self t2 classTrait) allTraits 
				= (self t1 classTrait + self t2 classTrait) copyTraitExpression allTraits.
	self assert: self t6 traitComposition allTraits 
				= self t6 traitComposition copyTraitExpression allTraits.
	self 
		assert: (self t6 asTraitComposition copyTraitExpression allTraits includesAll: { (self t1). (self t2). (self t6) }).
	"make no undue sharing happens of exclusions and aliases after an expression copy"
	t6compositionCopyFirst := self t6 traitComposition copyTraitExpression.
	t6compositionCopyFirst transformations at: 1 put: #m22Alias -> #m33.
	self 
		assert: self t6 traitComposition transformations second aliases first value 
				= #m22.
	c2compositionCopy := self c2 traitComposition copyTraitExpression.
	c2compositionCopy transformations first exclusions at: 1 put: #m4.
	self c2 traitComposition transformations first exclusions = #(#m11 )
]

{ #category : #testing }
TraitTest >> testErrorClassCreation [
    | tmpCategory trait aSubclass aClass |
	
    tmpCategory := 'TemporaryGeneratedClasses'.
    trait := Trait named: #TMyTrait uses: {} category: tmpCategory.
    
    [ aClass := nil
        subclass: #AClass
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: tmpCategory.    
	"----------------"
    aSubclass := aClass
        subclass: #AClass2
        uses: trait
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: tmpCategory.    
	"----------------"
    "Change the superclass of AClass"
    Object
        subclass: #AClass
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: tmpCategory.
        
    self assert: trait traitUsers asArray equals: {aSubclass}.
    self assert: aSubclass traits asArray equals: {trait}. 

    ] ensure: [
        #(AClass AClass2 TMyTrait) do: [ :aClassName |
            Smalltalk globals at: aClassName ifPresent: [ :v | v removeFromSystem ]]]
]

{ #category : #testing }
TraitTest >> testExplicitRequirement [

	self t1 compile: 'm self explicitRequirement'.
	self t2 compile: 'm ^true'.
	self deny: self t4 >> #m == (self t2 >> #m).
	self assert: self c2 new m.
	self t2 removeSelector: #m.
	self deny: self t5 >> #m == (self t1 >> #m).
	self should: [self c2 new m] raise: Error
]

{ #category : #testing }
TraitTest >> testExplicitRequirementDoesNotTakePrecedenceEvenWhenAddingTraits [
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation, even if he trait is created later."

	| tempClass tempTrait |
	self c9 compile: 'm ^100'.
	tempTrait := self createTraitNamed: #TTempTrait uses: {}.
	tempTrait compile: 'm ^self explicitRequirement.'.
	tempClass := self createClassNamed: #TempClass superclass: self c9 uses: {tempTrait}.
	self assert: tempClass new m equals: 100
]

{ #category : #testing }
TraitTest >> testExplicitRequirementDoesNotTakePrecedenceInDeepHierarchy [
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation, even if the superclass implementation is not just in the direct superclass."

	"C9 inherits from Object. C10 inherits from C9. C11 inherits from C10. Each Ci uses the trait Ti."

	self t11 compile: 'm ^self explicitRequirement'.
	self should: [ self c11 new m ] raise: Error.
	self t9 compile: 'm ^2'.
	self c9 new m.
	self c10 new m.
	self c11 new m.
	self assert: self c9 new m equals: 2.
	self assert: self c10 new m equals: 2.
	self assert: self c11 new m equals: 2
]

{ #category : #testing }
TraitTest >> testExplicitRequirementInClassAlwaysTakesPrecedence [
	"If i create an explicit requirement method on a class, and the superclass already have the method (non explicit requirement), the class should raise an exception"

	self c10 compile: 'm ^111'.
	self c11 compile: 'm ^self explicitRequirement'.
	self should: [ self c11 new m ] raise: Error
]

{ #category : #testing }
TraitTest >> testExplicitRequirementTakesPrecedenceOverTraitImplementation [
	"If i create an explicit requirement method on a trait, the method has to be explicit requirement "
	self t12 compile: 'm ^11'.
	self t11 compile: 'm ^self explicitRequirement'.
	
	self should: [ self c11 new m ] raise: Error.
	
	"If i create an explicit requirement method on a class, and the superclass already have the method (non explicit requirement), the class should raise an exception"
	self c10 compile: 'm ^111'.
	self c11 compile: 'm ^self explicitRequirement'.
	self should: [ self c11 new m ] raise: Error.
	
	
]

{ #category : #testing }
TraitTest >> testExplicitRequirementWithSuperclassImplementation [
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation."

	self t10 compile: 'm ^self explicitRequirement'.
	self should: [ self c10 new m ] raise: Error.
	self c9 compile: 'm ^123'.
	self assert: self c10 new m equals: 123.
	self c9 removeSelector: #m.
	self t10 removeSelector: #m.	"The previous is still true even if we compile the superclass method before the trait method."
	self c9 compile: 'm ^123'.
	self t10 compile: 'm ^self explicitRequirement'.
	self c10 new m.
	self c9 removeSelector: #m.
	self should: [ self c10 new m ] raise: Error.
	self t10 removeSelector: #m
]

{ #category : #testing }
TraitTest >> testExplicitRequirementWithSuperclassImplementationAndAnotherTrait [
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation."

	"The previous should still be true even in the presence of a superclass importing another explicitRequirement method."

	"C9 inherits from Object. C10 inherits from C9. C11 inherits from C10. Each Ci uses the trait Ti."

	self t10 compile: 'm ^self explicitRequirement'.
	self t11 compile: 'm ^self explicitRequirement'.
	self should: [ self c10 new m ] raise: Error.
	self should: [ self c11 new m ] raise: Error.
	self t9 compile: 'm ^2'.
	self assert: self c9 new m equals: 2.
	self assert: self c10 new m equals: 2.
	self assert: self c11 new m equals: 2
]

{ #category : #testing }
TraitTest >> testExplicitRequirementWithSuperclassImplementatiosAlwaysReturnsTheCorrectValue [
	"Make sure that an explicitRequirement method from a trait that is overrided by a method on a superclass always returns the value from the class defined method, even when an explict return from the trait defined method is missing."

	"C9 inherits from Object. C10 inherits from C9.  Each Ci uses the trait Ti."

	self t10 compile: 'm1 ^self explicitRequirement'.
	self t10 compile: 'm2 self explicitRequirement'.
	self t9 compile: 'm1 ^2'.
	self t9 compile: 'm2 ^2'.
	
	self assert: self c10 new m1 equals: 2.
	self assert: self c10 new m2 equals: 2.
]

{ #category : #testing }
TraitTest >> testIsRootInEnvironment [
	self assert: self t1 isRootInEnvironment.
	self assert: self t2 isRootInEnvironment
]

{ #category : #testing }
TraitTest >> testIsUsed [

	self assert: Trait1 isUsed.
	self assert: Trait1 classTrait isUsed.

]

{ #category : #testing }
TraitTest >> testLocalMethodWithSameCodeInTrait [
	"Test whether there are no unintentional overridden traits methods"

	SystemNavigation new allBehaviorsDo: [ :each |
		each hasTraitComposition ifTrue: [
			each selectorsDo: [ :selector |
				(each includesLocalSelector: selector) ifTrue: [
					(each traitComposition traitProvidingSelector: selector) ifNotNil: [ :trait |
						(trait selectors includes: selector) ifTrue: [
							self deny: (trait >> selector = (each >> selector)) ] ] ] ] ] ]
]

{ #category : #testing }
TraitTest >> testMarkerMethods [
	
	self t1 compile: 'm1 self foo bar'.
	self assert: (self t1 >> #m1) markerOrNil isNil.

	self t1 compile: 'm2 self requirement'.
	self assert: (self t1 >> #m2) markerOrNil == #requirement.
	
	self t1 compile: 'm3 ^self requirement'.
	self assert: (self t1 >> #m3) markerOrNil == #requirement.
]

{ #category : #testing }
TraitTest >> testOrigin [
	| tr1 tr2 tr3 tr23 aMethodDescription |
	tr1 := self createTraitNamed: #TTT1 uses: {}.
	tr2 := self createTraitNamed: #TTT2 uses: {tr1}.
	tr3 := self createTraitNamed: #TTT3 uses: {tr1}.
	tr23 := self createTraitNamed: #TTT23 uses: {tr3 + tr2}.
	
	tr1 compile: 'foo ^ 4'.
	self assert: (tr1 >> #foo) origin == tr1.
	self assert: (tr2 >> #foo) origin == tr1.
	self assert: (tr3 >> #foo) origin == tr1.
	
	"-----------"
	"For TR2"
	aMethodDescription := tr2 traitComposition methodDescriptionForSelector: #foo.
	self assert: (aMethodDescription locatedMethods size = 1).
	self assert: (aMethodDescription locatedMethods includes: ( tr1 >>  #foo)).

	self assert: (aMethodDescription providedLocatedMethod notNil).	
	"The method is provided, it cannot be nil"
	self assert: (aMethodDescription providedMethod notNil).	
	self assert: (aMethodDescription isProvided).	
		
	self assert: (tr2 traitComposition traitProvidingSelector: #foo) == tr1.
	self assert: (tr2 >> #foo) origin == tr1.
	"-----------"
	
	"-----------"
	"For TR23"
	aMethodDescription := tr23 traitComposition methodDescriptionForSelector: #foo.
	self assert: (aMethodDescription locatedMethods size = 2).
	self assert: (aMethodDescription locatedMethods includes: (tr2 >> #foo)).
	self assert: (aMethodDescription locatedMethods includes: (tr3 >> #foo)).

	self assert: (aMethodDescription providedLocatedMethod notNil).	
	"The method is provided, it cannot be nil"
	self assert: (aMethodDescription providedMethod notNil).	

	self assert: (aMethodDescription isProvided).	
		
	self assert: (tr23 traitComposition traitProvidingSelector: #foo) == tr1.
	self assert: (tr23 >> #foo) origin == tr1.
	"----------"
]

{ #category : #testing }
TraitTest >> testOriginWithRequiredMethod [
	| tr1 c1 |
	tr1 := self createTraitNamed: #TTT1 uses: {}.
	
	tr1 compile: 'foo ^ self explicitRequirement'.
	self assert: (tr1 >> #foo) origin == tr1.
	
	c1 := self createClassNamed: #CTT1 superclass: Object uses: tr1.
	self assert: (c1 >> #foo) origin == tr1.
]

{ #category : #testing }
TraitTest >> testOriginWithRequiredMethodInTraitChain [
	"check that the origin of a explicit requirement is the trait defining this method, even if we check
this from another *trait* using the first trait. Tr2 uses Tr1, Tr1 defines a explicit requirement #foo, the
origin of Tr2>>foo is Tr1"

	| tr1 tr2 |
	tr1 := self createTraitNamed: #TTT1 uses: {}.
	tr1 compile: 'foo ^ self explicitRequirement'.
	self assert: (tr1 >> #foo) origin == tr1.
	tr2 := self createTraitNamed: #TTT2 uses: {tr1}.
	self assert: (tr2 >> #foo) origin == tr1
]

{ #category : #testing }
TraitTest >> testPrinting [
	self assertPrints: self t6 definition
		like: 'Trait named: #T6
	uses: T1 + (T2 @ {#m22Alias->#m22})
	category: ''Tests-Traits'''
]

{ #category : #testing }
TraitTest >> testPrintingClassSide [
	
	self assertPrints: self t6 classSide definition
		like: 'T6 classTrait
	uses: T1 classTrait + T2 classTrait'
]

{ #category : #testing }
TraitTest >> testRemoveFromSystem [
	self flag: #fix.	"It has to be refactored when Mariano's ClassBuilder for testCase is integrated :)"
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ 
			| aClass aTrait |
			[ 
			aTrait := self createTrait.
			aClass := self createClassUsing: aTrait.
			aTrait removeFromSystem: false.
			self deny: aClass hasTraitComposition.
			self assert: aTrait isObsolete. ]
				ensure: [ aClass removeFromSystem: false ] ]
]

{ #category : #testing }
TraitTest >> testRequirement [
	
	self t1 compile: 'm self requirement'.
	self t2 compile: 'm ^true'.
	self assert: self c2 new m.
	self t2 removeSelector: #m.
	self should: [self c2 new m] raise: Error
]

{ #category : #testing }
TraitTest >> testTraitCompositionRespectsParenthesis [
	self t1 compile: 'method ^1234'.
	self t2 compile: 'method ^4'.
	self c1 setTraitComposition: self t1 + self t2 - #method.
	self should: [ self c1 new method ] raise: MessageNotUnderstood.
	self c1 setTraitComposition: self t1 + (self t2 - #method).
	self c1 new method.
	self assert: self c1 new method = 1234
]

{ #category : #testing }
TraitTest >> testTraitRemoval [
	self flag: #fix.	"It has to be refactored when Mariano's ClassBuilder for testCase is integrated :)"
	SystemAnnouncer uniqueInstance
		suspendAllWhile: [ 
			| aClass aTrait |
			[ 
			aTrait := self createTrait.
			aClass := self createClassUsing: aTrait.
			self assert: aClass localSelectors isEmpty.
			aClass removeFromComposition: aTrait.
			self assert: aClass localSelectors isEmpty.
			self assert: aClass organization allMethodSelectors isEmpty.
			self deny: aClass hasTraitComposition.
			self deny: (aTrait traitUsers includes: aClass). ]
				ensure: [ 
					aClass removeFromSystem: false.
					aTrait removeFromSystem: false ] ]
]

{ #category : #testing }
TraitTest >> testTraitsMethodClassSanity [
		
	(Smalltalk globals allTraits flatCollect: #traitUsers) asSet do: [ :each |
		each selectorsDo: [ :selector |
			self should: [ (each >> selector) methodClass == each ] ] ]
]

{ #category : #testing }
TraitTest >> testTraitsUsersSanity [
	"This documents bug http://code.google.com/p/pharo/issues/detail?id=443"
		
	Smalltalk allClassesAndTraits do: [ :each |
		self assert: (each traits allSatisfy: [ :t | t traitUsers includes: each  ]) ].

	Smalltalk allTraits do: [ :each |
		self assert: (each traitUsers allSatisfy: [ :b | b traits includes: each ]) ]
]

{ #category : #testing }
TraitTest >> testUsers [
	self assert: self t1 traitUsers size = 3.
	self assert: (self t1 traitUsers includesAll: {self t4. self t5. self t6 }).
	self assert: self t3 traitUsers isEmpty.
	self assert: self t5 traitUsers size = 1.
	self assert: self t5 traitUsers anyOne = self c2.
	self c2 setTraitCompositionFrom: self t1 + self t5.
	self assert: self t5 traitUsers size = 1.
	self assert: self t5 traitUsers anyOne = self c2.
	self c2 setTraitComposition: self t2 asTraitComposition.
	self assert: self t5 traitUsers isEmpty
]

{ #category : #testing }
TraitTest >> testUsersWithClassChanges [
	"This documents bug http://code.google.com/p/pharo/issues/detail?id=443"
		
	self c2 setTraitCompositionFrom: self t5.
	self assert: self t5 traitUsers size = 1.
	self assert: self t5 classSide traitUsers size = 1.
	self assert: self c2 traits size = 1.
	self assert: self c2 class traits size = 1.
	
	"Change class definition"
	self c2 addInstVarNamed: 'foo'.
	self assert: self t5 traitUsers size = 1.
	self assert: self t5 classSide traitUsers size = 1.
	self assert: self c2 traits size = 1.
	self assert: self c2 class traits size = 1.
	
	"Change metaclass definition"
	self c2 class instanceVariableNames: 'bar'.
	self assert: self t5 traitUsers size = 1.
	self assert: self t5 classSide traitUsers size = 1.
	self assert: self c2 traits size = 1.
	self assert: self c2 class traits size = 1.
]
