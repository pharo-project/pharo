Class {
	#name : #T2ObsoleteClassTest,
	#superclass : #T2AbstractTest,
	#category : #'TraitsV2-Tests'
}

{ #category : #tests }
T2ObsoleteClassTest >> testObsoleteClassIsRemovedFromUsers [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #()  uses:  t1 + t2.
		
	self assert: (t1 users includes: c1).
	self assert: (t2 users includes: c1).	
	
	c1 removeFromSystem.
	createdClasses remove: c1.
	
	self deny: (t1 users includes: c1).
	self deny: (t2 users includes: c1).	

]

{ #category : #tests }
T2ObsoleteClassTest >> testObsoleteClassIsRemovedFromUsersClassSide [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	c1 := self newClass: #C1 with: #()  uses: {}.
	c1 class setTraitComposition: t1 + t2. 
		
	self assert: (t1 users includes: c1 class).
	self assert: (t2 users includes: c1 class).	
	
	c1 removeFromSystem.
	createdClasses remove: c1.
	
	self deny: (t1 users includes: c1 class).
	self deny: (t2 users includes: c1 class).	

]

{ #category : #tests }
T2ObsoleteClassTest >> testObsoleteClassRemovesMethods [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 compile: 'm1 ^ 1'.

	t2 := self newTrait: #T2 with: #() uses: {}.
	t2 compile: 'm2 ^ 1'.

	c1 := self newClass: #C1 with: #()  uses:  t1 + t2.

	c1 compile: 'localM1 ^ 1'.

	self assert: (c1 methodDict includesKey: #m1).
	self assert: (c1 methodDict includesKey: #m2).
	self deny: (c1 localMethodDict includesKey: #m1).
	self deny: (c1 localMethodDict includesKey: #m2).
	self assert: (c1 localMethodDict includesKey: #localM1).
	
	c1 removeFromSystem.
	createdClasses remove: c1.
	
	self deny: (c1 methodDict includesKey: #m1).
	self deny: (c1 methodDict includesKey: #m2).
	
	"Accessing directly because there is no more accesor method"
	self assert: (c1 class instVarNamed: #baseLocalMethods) isEmpty.
	self deny: ((c1 class instVarNamed: #baseLocalMethods) includesKey: #m1).
	self deny: ((c1 class instVarNamed: #baseLocalMethods) includesKey: #m2).
	self deny: ((c1 class instVarNamed: #baseLocalMethods) includesKey: #localM1).

]

{ #category : #tests }
T2ObsoleteClassTest >> testObsoleteClassRemovesMethodsClassSide [
	| t1 t2 c1 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t1 compile: 'm1 ^ 1'.

	t2 := self newTrait: #T2 with: #() uses: {}.
	t2 compile: 'm2 ^ 1'.

	c1 := self newClass: #C1 with: #()  uses: {}.
	c1 class setTraitComposition: t1 + t2. 

	c1 class compile: 'localM1 ^ 1'.

	self assert: (c1 class methodDict includesKey: #m1).
	self assert: (c1 class methodDict includesKey: #m2).
	self deny: (c1 class localMethodDict includesKey: #m1).
	self deny: (c1 class localMethodDict includesKey: #m2).
	self assert: (c1 class localMethodDict includesKey: #localM1).
	
	c1 removeFromSystem.
	createdClasses remove: c1.
	
	self deny: (c1 class methodDict includesKey: #m1).
	self deny: (c1 class methodDict includesKey: #m2).
	self deny: (c1 class localMethodDict includesKey: #m1).
	self deny: (c1 class localMethodDict includesKey: #m2).
	self deny: (c1 class localMethodDict includesKey: #localM1).

]

{ #category : #tests }
T2ObsoleteClassTest >> testObsoleteTraitIsRemovedFromUsers [
	| t1 t2 t3 |
	t1 := self newTrait: #T1 with: #() uses: {}.
	t2 := self newTrait: #T2 with: #() uses: {}.

	t3 := self newTrait: #T3 with: #()  uses:  t1 + t2.
		
	self assert: (t1 users includes: t3).
	self assert: (t2 users includes: t3).	
	
	t3 removeFromSystem.
	createdClasses remove: t3.
	
	self deny: (t1 users includes: t3).
	self deny: (t2 users includes: t3).	

]
