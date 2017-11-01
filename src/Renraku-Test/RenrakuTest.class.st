Class {
	#name : #RenrakuTest,
	#superclass : #ReCodeBasedTestCase,
	#category : 'Renraku-Test'
}

{ #category : #tests }
RenrakuTest >> testAsClassRuleTransformation [
	| asClassMethod critique |
	
	asClassMethod := testClass >> (testClass compile:
		'aMethodWhichSendsAsClass 
			#', self class name, ' asClass').
	
	critique := asClassMethod critiques detect: [ :crit |
		crit rule class = AsClassRule ].
	
	self assert: critique change class equals: RBAddMethodChange.
	self assert: (critique change source includesSubstring: 'self class environment at:')
	
]

{ #category : #tests }
RenrakuTest >> testBan [
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testClass ban: criticToSkip.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuTest >> testBan1 [
	| criticToSkip |
	
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testMethod ban: criticToSkip.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuTest >> testBan2 [
	| criticToSkip |
	
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testClass ban: criticToSkip.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuTest >> testBanForPackage [
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	testPackage ban: criticToSkip.
	
	self assert: (testClass2 critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
RenrakuTest >> testNoClassComment [
		
	self assert: (testClass critiques anySatisfy: [ :critic |
		critic rule class = RBNoClassCommentRule ]).
	
	testClass comment: 'Now I have a comment'.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = RBNoClassCommentRule ]).
]

{ #category : #tests }
RenrakuTest >> testUnclassifiedMethod [

	self assert: (testMethod critiques anySatisfy: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ]).
	
	testMethod protocol: 'some protocol'.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = RBUnclassifiedMethodsRule ]).
]
