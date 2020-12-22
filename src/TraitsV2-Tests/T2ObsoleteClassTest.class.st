"
Unit test for trait handling when classes are removed
"
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
