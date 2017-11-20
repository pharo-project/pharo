Class {
	#name : #ReCritiqueTest,
	#superclass : #ReCodeBasedTestCase,
	#category : #Renraku-Test
}

{ #category : #tests }
ReCritiqueTest >> testBan [
	| criticToSkip |
		
	criticToSkip := testClass critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	criticToSkip ban.
	
	self assert: (testClass critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
ReCritiqueTest >> testBan1 [
	| criticToSkip |
	
		
	criticToSkip := testMethod critiques detect: [ :critic |
		critic rule class = ScreamerRule ].
	
	criticToSkip ban.
	
	self assert: (testMethod critiques noneSatisfy: [ :critic |
		critic rule class = ScreamerRule ]).
]

{ #category : #tests }
ReCritiqueTest >> testDescription [
	| rule critic |
	
	rule := (RBLintRule allSubclasses select: #isVisible)	 anyOne new.
	critic := ReAbstractCritique for: thisContext method by: rule.
	
	self assert: critic description equals: rule rationale
]

{ #category : #tests }
ReCritiqueTest >> testRule [
	| rule critic |
	
	rule := (RBLintRule allSubclasses select: #isVisible)	 anyOne new.
	critic := ReAbstractCritique for: thisContext method by: rule.
	
	self assert: critic rule equals: rule
]

{ #category : #tests }
ReCritiqueTest >> testTarget [
	| rule critic |
	
	rule := (RBLintRule allSubclasses select: #isVisible)	 anyOne new.
	critic := ReAbstractCritique for: thisContext method by: rule.
	
	self assert: critic sourceAnchor entity equals: thisContext method
]

{ #category : #tests }
ReCritiqueTest >> testTitle [
	| rule critic |
	
	rule := (RBLintRule allSubclasses select: #isVisible)	 anyOne new.
	critic := ReAbstractCritique for: thisContext method by: rule.
	
	self assert: critic title equals: rule name
]
