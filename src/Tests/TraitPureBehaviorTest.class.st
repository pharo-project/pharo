Class {
	#name : #TraitPureBehaviorTest,
	#superclass : #TraitsTestCase,
	#category : #'Tests-Traits'
}

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testChangeSuperclass [
	"Test that when the superclass of a class is changed the non-local methods
	of the class sending super are recompiled to correctly store the new superclass."

	| aC2 newSuperclass |
	aC2 := self c2 new.

	"C1 is current superclass of C2"
	self assert: aC2 m51.
	self assert: self c2 superclass == self c1.
	self deny: (self c2 localSelectors includes: #m51).
	self deny: (self c2 >> #m52) == (self t5 >> #m52). "no sharing!!"
	
	self assert: self c2 traitCompositionString equals: 'T5 - {#m11}'.
	self assert: self c2 selectors sorted equals: #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted equals: #(bar foo).

	"change superclass of C2 from C1 to X"
	newSuperclass := self createClassNamed: #X superclass: Object uses: {}.
	newSuperclass
		subclass: self c2 name
		uses: self c2 traitComposition
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self c2 category.

	self assert: self c2 superclass == newSuperclass.

	newSuperclass compile: 'foo ^17'.
	self assert: aC2 m51 equals: 17.
	self deny: (self c2 localSelectors includes: #m51).

	self c2 compile: 'm51 ^19'.
	self assert: aC2 m51 equals: 19.
	
	self deny: (self c2 >> #m52) == (self t5 >> #m52). "no sharing!!"

	self assert: self c2 traitCompositionString equals: 'T5 - {#m11}'.
	self assert: self c2 selectors sorted equals: #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted equals: #(bar foo m51).

	"change superclass of C2 back to C1"
	self c1
		subclass: self c2 name
		uses: self c2 traitComposition
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self c2 category.

	self assert: aC2 m51 equals: 19.
	self assert: self c2 superclass == self c1.
	self assert: (self c2 localSelectors includes: #m51).
	self deny: (self c2 >> #m52) == (self t5 >> #m52). "no sharing!!"

	self assert: self c2 traitCompositionString equals: 'T5 - {#m11}'.
	self assert: self c2 selectors sorted equals: #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted equals: #(bar foo m51).
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testClassesWithTraits [	

	self assert: (self c1 methodDict includesKey: #foo).
	self assert: (self c2 methodDict includesKey: #bar).
	self assert: (self c2 methodDict includesKey: #m51).
	self assert: (self c2 methodDict includesKey: #m12).
	self assert: (self c2 methodDict includesKey: #m13).
	self assert: (self c2 methodDict includesKey: #m21).
	self assert: (self c2 methodDict includesKey: #m22).
	
	self deny: self c1 class hasTraitComposition.
	self assert: self c2 class hasTraitComposition.

	self assert: (self c2 class traitComposition size = 1).
	self assert: (self c2 class traitComposition includesTrait: self t5 classTrait)
]

{ #category : #testing }
TraitPureBehaviorTest >> testIsAliasSelector [
	self deny: (self t1 isAliasSelector: #m11).
	self deny: (self t1 isAliasSelector: #foo).

	"directly"
	self assert: (self t6 isAliasSelector: #m22Alias).
	self deny: (self t6 isAliasSelector: #m22).

	"indirectly"
	self c1 setTraitCompositionFrom: self t6.
	self assert: (self c1 isAliasSelector: #m22Alias).
	self deny: (self c1 isAliasSelector: #m22)
]

{ #category : #testing }
TraitPureBehaviorTest >> testIsLocalAliasSelector [
	self deny: (self t1 isLocalAliasSelector: #m11).
	self deny: (self t1 isLocalAliasSelector: #foo).

	"directly"
	self assert: (self t6 isLocalAliasSelector: #m22Alias).
	self deny: (self t6 isLocalAliasSelector: #m22).

	"indirectly"
	self c1 setTraitComposition: self t6 asTraitComposition.
	self deny: (self c1 isLocalAliasSelector: #m22Alias).
	self deny: (self c1 isLocalAliasSelector: #m22)
]

{ #category : #testing }
TraitPureBehaviorTest >> testLocalSelectors [

	self assert: self t3 localSelectors size equals: 3.
	self assert: (self t3 localSelectors includesAll: #(#m31 #m32 #m33 )).
	self assert: (self t3 includesLocalSelector: #m32).
	self deny: (self t3 includesLocalSelector: #inexistantSelector).
	self assert: self t5 localSelectors size equals: 3.
	self assert: (self t5 localSelectors includes: #m51).
	self assert: (self t5 includesLocalSelector: #m51).
	self deny: (self t5 includesLocalSelector: #m11).
	self t5 removeSelector: #m51.
	self deny: (self t3 includesLocalSelector: #m51).
	self deny: (self t5 includesLocalSelector: #m11).
	self assert: self t5 localSelectors size equals: 2.
	self t5 compile: 'm52 ^self'.
	self assert: self t5 localSelectors size equals: 2.
	self assert: (self t5 localSelectors includes: #m52).

	"test that propagated methods do not get in as local methods"
	self t2 compile: 'local2 ^self'.
	self deny: (self t5 includesLocalSelector: #local2).
	self assert: self t5 localSelectors size equals: 2.
	self assert: (self t5 localSelectors includes: #m52).
	self assert: self c2 localSelectors size equals: 2.
	self assert: (self c2 localSelectors includesAll: #(#foo #bar ))
]

{ #category : #testing }
TraitPureBehaviorTest >> testMethodCategoryReorganization [

	self t1 compile: 'm1' classified: 'category1'.
	self assert: (self t5 organization categoryOfElement: #m1) equals: #category1.
	self assert: (self c2 organization categoryOfElement: #m1) equals: #category1.
	self t1 organization 
		classify: #m1
		under: #category2
		suppressIfDefault: true.
	self assert: (self t5 organization categoryOfElement: #m1) equals: #category2.
	self assert: (self c2 organization categoryOfElement: #m1) equals: #category2
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testMethodClass [
	"Test sharing of compiled methods between traits and their users. Methods are installed in exactly one behavior, however, the source pointers of methods are shared (unless sources or changes have been condensed). Verify	that methodClass properties are set correctly."
	| m1 m2 |
	m1 := self t5 >> #m51.
	m2 := self c2 >> #m51.
	
	self assert: m1 methodClass == self t5.
	self assert: m2 methodClass == self c2.
	
	self deny: m1 == m2.
	
	self deny: m1 sourcePointer = m2 sourcePointer.
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testOwnMethodsTakePrecedenceOverTraitsMethods [
	"First create a trait with no subtraits and then
	add subtrait t1 which implements m11 as well."

	| trait |
	trait := self createTraitNamed: #T
				uses: { }.
	trait compile: 'm11 ^999'.
	self assert: trait methodDict size = 1.
	self assert: (trait methodDict at: #m11) sourceCode = 'm11 ^999'.
	Trait 
		named: #T
		uses: self t1
		category: self class category.
	self assert: trait methodDict size = 3.
	self assert: (trait selectors asSet includesAll: #(#m11 #m12 #m13 )).
	self assert: (trait methodDict at: #m11) sourceCode = 'm11 ^999'.
	self assert: (trait methodDict at: #m12) sourceCode = 'm12 ^12'
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testPropagationOfChangesInTraits [
	| aC2 |
	aC2 := self c2 new.
	self assert: self c2 methodDict size = 9.
	self t1 compile: 'zork ^false'.
	self assert: self c2 methodDict size = 10.
	self deny: aC2 zork.
	self t1 removeSelector: #m12.
	self assert: self c2 methodDict size = 9.
	self should: [aC2 m12] raise: MessageNotUnderstood.
	self assert: aC2 m21 = 21.
	self t2 compile: 'm21 ^99'.
	self assert: aC2 m21 = 99
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testPropagationOfChangesInTraitsToAliasMethods [
	| anObject |
	anObject := (self 
				createClassNamed: #AliasTestClass
				superclass: Object
				uses: self t6) new.
	self assert: anObject m22Alias = 22.

	"test update alias method"
	self t2 compile: 'm22 ^17'.
	self assert: anObject m22Alias = 17.

	"removing original method should also remove alias method"
	self t2 removeSelector: #m22.
	self should: [anObject m22Alias] raise: MessageNotUnderstood
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testPropagationOfChangesInTraitsToAliasMethodsWhenOriginalMethodIsExcluded [
	"Assert that alias method is updated although
	the original method is excluded from this user."

	| anObject |
	anObject := (self 
				createClassNamed: #AliasTestClass
				superclass: Object
				uses: self t1 @ { (#aliasM11 -> #m11) } - { #m11 }) new.
	self assert: anObject aliasM11 = 11.
	self deny: (anObject class methodDict includesKey: #m11).
	self t1 compile: 'm11 ^17'.
	self assert: anObject aliasM11 = 17
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testPropagationWhenTraitCompositionModifications [
	"Test that the propagation mechanism works when
	setting new traitCompositions."

	self assert: self c2 methodDict size = 9.	"2 + (3+(3+2))-1"

	"removing methods"
	Trait 
		named: #T5
		uses: self t1 + self t2 - { #m21. #m22 }
		category: self class category.
	self assert: self c2 methodDict size = 7.

	"adding methods"
	Trait 
		named: #T2
		uses: self t3
		category: self class category.
	self assert: self c2 methodDict size = 10.
	self assert: (self c2 selectors asSet includesAll: #(#m31 #m32 #m33 ))
]

{ #category : #testing }
TraitPureBehaviorTest >> testRemovingMethods [
	"When removing a local method, assure that the method
	from the trait is installed instead and that the users are 
	updated."

	"Classes"
	self c2 compile: 'm12 ^0' classified: #xxx.
	self assert: (self c2 includesLocalSelector: #m12).
	self c2 removeSelector: #m12.
	self deny: (self c2 includesLocalSelector: #m12).
	self assert: (self c2 selectors includes: #m12).

	"Traits"
	self t5 compile: 'm12 ^0' classified: #xxx.
	self assert: self c2 new m12 equals: 0.
	self t5 removeSelector: #m12.
	self deny: (self t5 includesLocalSelector: #m12).
	self assert: (self t5 selectors includes: #m12).
	self assert: self c2 new m12 equals: 12
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testReshapeClass [
	"Ensure that reshaping a class has no impact on its traits"

	self assert: self c2 traitCompositionString equals: 'T5 - {#m11}'.
	self assert: self c2 selectors sorted equals: #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted equals: #(bar foo).

	self c2 addInstVarNamed: 'foobar'.

	self assert: self c2 traitCompositionString equals: 'T5 - {#m11}'.
	self assert: self c2 selectors sorted equals: #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted equals: #(bar foo).

	self c2 removeInstVarNamed: 'foobar'.

	self assert: self c2 traitCompositionString equals: 'T5 - {#m11}'.
	self assert: self c2 selectors sorted equals: #(bar foo m12 m13 m21 m22 m51 m52 m53).
	self assert: self c2 localSelectors sorted equals: #(bar foo).
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testSuperSends [

	| aC2 |
	aC2 := self c2 new.
	self assert: aC2 m51.
	self deny: aC2 foo.
	self deny: aC2 bar
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testTraitCompositionModifications [
	self assert: self t6 methodDict size = 6.
	self assert: (self t6 sourceCodeAt: #m22Alias) asString = 'm22Alias ^22'.
	self t6 setTraitComposition: self t2 asTraitComposition.
	self assert: self t6 methodDict size = 2.
	self deny: (self t6 methodDict includesKey: #m22Alias).
	self t6 
		setTraitCompositionFrom: self t1 @ { (#m13Alias -> #m13) } - { #m11. #m12 } 
				+ self t2.
	self assert: self t6 methodDict size = 4.
	self 
		assert: (self t6 selectors asSet includesAll: #(#m13 #m13Alias #m21 #m22 )).
	self 
		assert: (self t6 sourceCodeAt: #m13Alias) asString = 'm13Alias ^self m12'
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testTraitCompositionWithCycles [
	self should: [self t1 setTraitComposition: self t1 asTraitComposition]
		raise: Error.
	self t2 setTraitComposition: self t3 asTraitComposition.
	self should: [self t3 setTraitComposition: self t5 asTraitComposition]
		raise: Error
]

{ #category : #testing }
TraitPureBehaviorTest >> testTraitsAccessor [
	
	self assert: self c1 traits isEmpty.
	self assert: (self c2 traits hasEqualElements: (Array with: self t5))
]

{ #category : #'testing-applying trait composition' }
TraitPureBehaviorTest >> testUpdateWhenLocalMethodRemoved [
	| aC2 |
	aC2 := self c2 new.
	self t5 compile: 'foo ^123'.
	self deny: aC2 foo.
	self c2 removeSelector: #foo.
	self assert: aC2 foo = 123
]

{ #category : #testing }
TraitPureBehaviorTest >> traitOrClassOfSelector [

	"locally defined in trait or class"
	self assert: (self t1 traitOrClassOfSelector: #m12) equals: self t1.
	self assert: (self c1 traitOrClassOfSelector: #foo) equals: self c1.

	"not locally defined - simple"
	self assert: (self t4 traitOrClassOfSelector: #m21) equals: self t2.
	self assert: (self c2 traitOrClassOfSelector: #m51) equals: self t5.

	"not locally defined - into nested traits"
	self assert: (self c2 traitOrClassOfSelector: #m22) equals: self t2.

	"not locally defined - aliases"
	self assert: (self t6 traitOrClassOfSelector: #m22Alias) equals: self t2.

	"class side"
	self assert: (self t2 classSide traitOrClassOfSelector: #m2ClassSide:) 
		  equals: self t2 classSide.
	self assert: (self t6 classSide traitOrClassOfSelector: #m2ClassSide:) 
		  equals: self t2 classSide
]
