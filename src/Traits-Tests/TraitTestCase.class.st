Class {
	#name : 'TraitTestCase',
	#superclass : 'AbstractTraitsOnPreparedModelTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitTestCase >> testAddAndRemoveMethodsFromSubtraits [
	| aC2 |
	<ignoreNotImplementedSelectors: #(m51)>
	aC2 := self c2 new.
	self assert: aC2 m51.
	self t5 removeSelector: #m51.
	self should: [ aC2 m51 ] raise: MessageNotUnderstood.
	self t1 compile: 'foo ^true'.
	self deny: aC2 foo.
	self t1 compile: 'm51 ^self'.
	aC2 m51.
	self assert: aC2 m51 identicalTo: aC2
]

{ #category : 'tests' }
TraitTestCase >> testAddAndRemoveMethodsInClassOrTrait [
	| aC2 |
	<ignoreNotImplementedSelectors: #(m51)>
	aC2 := self c2 new.
	self assert: aC2 m51.
	self c2 compile: 'm51 ^123'.
	self assert: aC2 m51 equals: 123.
	self c2 removeSelector: #m51.
	aC2 m51.
	self assert: aC2 m51.
	self t4 removeSelector: #m11.
	self assert: (self t4 includesSelector: #m11)
]

{ #category : 'tests' }
TraitTestCase >> testAllClassVarNames [
	self assertEmpty: self t1 allClassVarNames
]

{ #category : 'tests' }
TraitTestCase >> testCompositionCopy [

	self assert: (self t1 + self t2) allTraits
				equals: (self t1 + self t2) copyTraitExpression allTraits.
	self assert: (self t1 classTrait + self t2 classTrait) allTraits
				equals: (self t1 classTrait + self t2 classTrait) copyTraitExpression allTraits.
	self assert: self t6 traitComposition allTraits
				equals: self t6 traitComposition copyTraitExpression allTraits.
	self assert: (self t6 asTraitComposition copyTraitExpression allTraits
				includesAll: { (self t1). (self t2). (self t6) })
]

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirement [
	self t1 compile: 'm self explicitRequirement'.
	self t2 compile: 'm ^true'.
	self deny: self t4 >> #m identicalTo: self t2 >> #m.
	self assert: self c2 new m.
	self t2 removeSelector: #m.
	self deny: self t5 >> #m identicalTo: self t1 >> #m.
	self should: [ self c2 new m ] raise: Error
]

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirementDoesNotTakePrecedenceEvenWhenAddingTraits [
	"Make sure that an explicitRequirement method from a trait does not take precedence over a superclass implementation, even if he trait is created later."

	| tempClass tempTrait |
	self c9 compile: 'm ^100'.
	tempTrait := self newTrait: #TTempTrait traits: {  }.
	tempTrait compile: 'm ^self explicitRequirement.'.
	tempClass := self newClass: #TempClass superclass: self c9 traits: { tempTrait }.
	self assert: tempClass new m equals: 100
]

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirementDoesNotTakePrecedenceInDeepHierarchy [
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

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirementInClassAlwaysTakesPrecedence [
	"If i create an explicit requirement method on a class, and the superclass already have the method (non explicit requirement), the class should raise an exception"

	self c10 compile: 'm ^111'.
	self c11 compile: 'm ^self explicitRequirement'.
	self assert: self c11 new m equals: 111
]

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirementTakesPrecedenceOverTraitImplementation [

	"If I create an explicit requirement method on a trait, the method has to be explicit requirement "
	self t12 compile: 'm ^11'.
	self t11 compile: 'm ^self explicitRequirement'.

	self should: [ self c11 new m ] raise: Error.

	"If I create an explicit requirement method on a class, and the superclass already have the method (non explicit requirement), the class execute what is in the superclass"
	self c10 compile: 'm ^111'.
	self c11 compile: 'm ^self explicitRequirement'.
	self assert: self c11 new m equals: 111
]

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirementWithSuperclassImplementation [
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

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirementWithSuperclassImplementationAndAnotherTrait [
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

{ #category : 'tests' }
TraitTestCase >> testExplicitRequirementWithSuperclassImplementatiosAlwaysReturnsTheCorrectValue [
	"Make sure that an explicitRequirement method from a trait that is overrided by a method on a superclass always returns the value from the class defined method, even when an explict return from the trait defined method is missing."

	"C9 inherits from Object. C10 inherits from C9.  Each Ci uses the trait Ti."


	<ignoreNotImplementedSelectors: #(m1 m2)>
	self t10 compile: 'm1 ^self explicitRequirement'.
	self t10 compile: 'm2 self explicitRequirement'.
	self t9 compile: 'm1 ^2'.
	self t9 compile: 'm2 ^2'.

	self assert: self c10 new m1 equals: 2.
	self assert: self c10 new m2 equals: 2
]

{ #category : 'tests' }
TraitTestCase >> testIsRootInEnvironment [
	self assert: self t1 isRootInEnvironment.
	self assert: self t2 isRootInEnvironment
]

{ #category : 'tests' }
TraitTestCase >> testMarkerMethods [
	self t1 compile: 'm1 self foo bar'.
	self assert: (self t1 >> #m1) markerOrNil isNil.

	self t1 compile: 'm2 self explicitRequirement'.
	self assert: (self t1 >> #m2) markerOrNil identicalTo: #explicitRequirement
]

{ #category : 'tests' }
TraitTestCase >> testPrinting [

	self assertPrints: (ClassDefinitionPrinter oldPharo for: self t6) definitionString like: 'Trait named: #T6
	uses: T1 + (T2 @ {#m22Alias->#m22})
	instanceVariableNames:''''
	package: ''' , self packageNameForTests , ''''
]

{ #category : 'tests' }
TraitTestCase >> testPrintingClassSide [

	self
		assertPrints: (ClassDefinitionPrinter oldPharo for: self t6 classSide) definitionString like: 'T6 classTrait
	uses: T1 classTrait + T2 classTrait
	instanceVariableNames: '''''
]

{ #category : 'tests' }
TraitTestCase >> testTraitCompositionRespectsParenthesis [
	self t1 compile: 'method ^1234'.
	self t2 compile: 'method ^4'.
	self c1 setTraitComposition: self t1 + self t2 - #method.
	self should: [ self c1 new method ] raise: MessageNotUnderstood.
	self c1 setTraitComposition: self t1 + (self t2 - #method).
	self c1 new method.
	self assert: self c1 new method equals: 1234
]

{ #category : 'tests' }
TraitTestCase >> testUsers [
	self assert: self t1 traitUsers size equals: 3.
	self assert: (self t1 traitUsers includesAll: {self t4 . self t5 . self t6}).
	self assertEmpty: self t3 traitUsers.
	self assert: self t5 traitUsers size equals: 1.
	self assert: self t5 traitUsers anyOne equals: self c2.
	self c2 setTraitComposition: self t1 + self t5.
	self assert: self t5 traitUsers size equals: 1.
	self assert: self t5 traitUsers anyOne equals: self c2.
	self c2 setTraitComposition: self t2 asTraitComposition.
	self assertEmpty: self t5 traitUsers
]

{ #category : 'tests' }
TraitTestCase >> testUsersWithClassChanges [
	"This documents bug http://code.google.com/p/pharo/issues/detail?id=443"

	"self debug: #testUsersWithClassChanges"

	self c2 setTraitComposition: self t5 asTraitComposition.
	self assert: self t5 traitUsers size equals: 1.
	self assert: self t5 classSide traitUsers size equals: 1.
	self assert: self c2 traits size equals: 1.
	self assert: self c2 class traits size equals: 1.

	"Change class definition"
	self c2 addInstVarNamed: 'foo'.
	self assert: self t5 traitUsers size equals: 1.
	self assert: self t5 classSide traitUsers size equals: 1.
	self assert: self c2 traits size equals: 1.
	self assert: self c2 class traits size equals: 1.

	"Change metaclass definition"
	self c2 class instanceVariableNames: 'bar'.
	self assert: self t5 traitUsers size equals: 1.
	self assert: self t5 classSide traitUsers size equals: 1.
	self assert: self c2 traits size equals: 1.
	self assert: self c2 class traits size equals: 1
]
