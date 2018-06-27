"
Unit test for categorization of traits
"
Class {
	#name : #T2TraitWithCategories,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests-Tests'
}

{ #category : #tests }
T2TraitWithCategories >> testPackageOfMethodFromTraits [
	| t1 t2 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.
	
	t2 := self newTrait: #T2 with: #() uses: t1.	

	self assert: (t1 >> #m1) protocol equals: 'aProtocol'.
	self assert: (t2 >> #m1) protocol equals: 'aProtocol'.
	
]

{ #category : #tests }
T2TraitWithCategories >> testPackageOfMethodFromTraitsAfterCreation [
	| t1 t2 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: t1.	
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.
	
	self assert: (t1 >> #m1) protocol equals: 'aProtocol'.
	self assert: (t2 >> #m1) protocol equals: 'aProtocol'.
	
]

{ #category : #tests }
T2TraitWithCategories >> testPackageOfMethodFromTraitsChanged [
	| t1 t2 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.
	
	t2 := self newTrait: #T2 with: #() uses: t1.	

	t1 compile: 'm1 ^42.' classified: 'anotherProtocol'.

	self assert: (t1 >> #m1) protocol equals: 'anotherProtocol'.
	self assert: (t2 >> #m1) protocol equals: 'anotherProtocol'.
	
]

{ #category : #tests }
T2TraitWithCategories >> testPackageOfMethodFromTraitsChangedWithoutCompile [
	| t1 t2 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.
	
	t2 := self newTrait: #T2 with: #() uses: t1.	

	t1 organization classify: #m1 under: 'anotherProtocol'.

	self assert: (t1 >> #m1) protocol equals: 'anotherProtocol'.
	self assert: (t2 >> #m1) protocol equals: 'anotherProtocol'.
	
]

{ #category : #tests }
T2TraitWithCategories >> testPackageOfMethodFromTraitsOverriden [
	| t1 t2 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.
	
	t2 := self newTrait: #T2 with: #() uses: t1.	
	t2 compile: 'm1 ^42.' classified: 'anotherProtocol'.
	
	self assert: (t1 >> #m1) protocol equals: 'aProtocol'.
	self assert: (t2 >> #m1) protocol equals: 'anotherProtocol'.
	
]

{ #category : #tests }
T2TraitWithCategories >> testPackageOfMethodFromTraitsRenamedCategory [
	| t1 t2 |
	
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 compile: 'm1 ^42.' classified: 'aProtocol'.
	t1 compile: 'm2 ^42.' classified: 'aProtocol'.
	
	t2 := self newTrait: #T2 with: #() uses: t1.	
	
	t1 organization renameCategory: 'aProtocol' toBe: 'anotherProtocol'.
	
	self assert: (t1 >> #m1) protocol equals: 'anotherProtocol'.
	self assert: (t2 >> #m1) protocol equals: 'anotherProtocol'.
	self assert: (t1 >> #m2) protocol equals: 'anotherProtocol'.
	self assert: (t2 >> #m2) protocol equals: 'anotherProtocol'.
	
]
