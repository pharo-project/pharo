Class {
	#name : 'TraitSubclassingTraitedClassTest',
	#superclass : 'TraitAbstractTest',
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testCreatingMethodInSubclass [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	c2 compile: 'asd'.

	self shouldnt: [ c2 >> #asd ] raise: Error.
	self assert: (c2 >> #asd) package name equals: self packageNameForTests.
	self assert: (c2 >> #asd) package equals: c2 package
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testCreatingMethodInSubclass2 [

	| t1 c1 c2 |
	c1 := self newClass: #C1.
	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 compile: 'asd'.

	self shouldnt: [ c2 >> #asd ] raise: Error.
	self assert: (c2 >> #asd) package name equals: self packageNameForTests.
	self assert: (c2 >> #asd) package equals: c2 package
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testCreatingMethodInTraitClassSide [
	| t1 c1 |
	t1 := self newTrait: #T1.
	t1 class compile: 'someObject ^#executingOverridenMethod'.
	c1 := self newClass: #C1 traits: t1.
	self assert: c1 someObject equals: #executingOverridenMethod
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testRedefinedTraitedClassSubclassClassTraitCompositionIncludesTraitedMetaclass [
	| t1 c1 c2 |

	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.
	
	"Redefine it"
	self class classInstaller make: [ :aClassBuilder |
		aClassBuilder
			superclass: c1;
			name: #C2;
			fillClassSideFromEnvironment: self class environment;
			yourself ].

	self 
		assert: c2 class traitComposition 
		equals: {} asTraitComposition
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testRedefinedTraitedClassSubclassClassTraitCompositionIncludesTraitedMetaclassAPI2 [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	"Redefine it"
	self class classInstaller make: [ :aClassBuilder |
		aClassBuilder fillFor: c2 ].

	self
		assert: c2 class traitComposition
		equals: {  } asTraitComposition
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testRedefinedTraitedClassSubclassHasEmptyClassTraitComposition [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	"Redefine it"
	self class classInstaller make: [ :aClassBuilder |
		aClassBuilder
			superclass: c1;
			name: #C2;
			fillClassSideFromEnvironment: self class environment;
			yourself ].

	self assert: c2 class basicTraitComposition isEmpty
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testRedefinedTraitedClassSubclassHasEmptyClassTraitCompositionAPI2 [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	"Redefine it"
	self class classInstaller make: [ :aClassBuilder |
		aClassBuilder fillFor: c2 ].

	self assert: c2 class basicTraitComposition isEmpty
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testRedefinedTraitedClassSubclassHasEmptyTraitComposition [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	"Redefine it"
	self class classInstaller make: [ :aClassBuilder |
		aClassBuilder
			superclass: c1;
			name: #C2;
			fillClassSideFromEnvironment: self class environment;
			yourself ].

	self assert: c2 traitComposition isEmpty
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testRedefinedTraitedClassSubclassHasEmptyTraitCompositionAPI2 [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	"Redefine it"
	self class classInstaller make: [ :aClassBuilder |
		aClassBuilder fillFor: c2 ].

	self assert: c2 traitComposition isEmpty
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testTraitedClassSubclassClassTraitCompositionIncludesTraitedMetaclass [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	self 
		assert: c2 class traitComposition 
		equals: {} asTraitComposition
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testTraitedClassSubclassHasEmptyClassTraitComposition [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	self assert: c2 class basicTraitComposition isEmpty
]

{ #category : 'tests' }
TraitSubclassingTraitedClassTest >> testTraitedClassSubclassHasEmptyTraitComposition [

	| t1 c1 c2 |
	t1 := self newTrait: #T1.
	c1 := self newClass: #C1 traits: t1.

	c2 := self newClass: #C2 superclass: c1 traits: {  }.

	self assert: c2 traitComposition isEmpty
]
