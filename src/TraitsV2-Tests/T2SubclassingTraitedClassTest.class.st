Class {
	#name : #T2SubclassingTraitedClassTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2SubclassingTraitedClassTest >> testCreatingMethodInSubclass [
	| t1 c1 c2 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	c1 := self newClass: #C1 with: #()  uses: t1.

	c2 := self newClass: #C2 superclass: c1 with: #() uses: {}.

	c2 compile: 'asd'.

	self shouldnt: [c2 >> #asd] raise: Error.
	self assert: (c2 >> #asd) package name equals: 'TraitsV2-Tests'.
	self assert: (c2 >> #asd) package equals: c2 package.
]

{ #category : #tests }
T2SubclassingTraitedClassTest >> testCreatingMethodInSubclass2 [
	| t1 c1 c2 |
	c1 := self newClass: #C1 with: #() uses: {}.
	c2 := self newClass: #C2 superclass: c1 with: #() uses: {}.

	t1 := self newTrait: #T1 with: #() uses: {}.
	c1 := self newClass: #C1 with: #() uses: t1.

	c2 compile: 'asd'.

	self shouldnt: [c2 >> #asd] raise: Error.
	self assert: (c2 >> #asd) package name equals: 'TraitsV2-Tests'.
	self assert: (c2 >> #asd) package equals: c2 package.
]

{ #category : #tests }
T2SubclassingTraitedClassTest >> testCreatingMethodInTraitClassSide [
	| t1 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 class compile: 'someObject ^#executingOverridenMethod'.	
	c1 := self newClass: #C1 with: #() uses: t1.
	self assert: c1 someObject equals: #executingOverridenMethod.
]
