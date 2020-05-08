Class {
	#name : #ReTestClassNameShouldEndWithTestTest,
	#superclass : #ReTestBasedTestCase,
	#category : #'Renraku-Tests'
}

{ #category : #tests }
ReTestClassNameShouldEndWithTestTest >> testBasicCheck [
	self assert: (testClass critiques noneSatisfy: [ :critic | critic rule class = ReTestClassNameShouldEndWithTestRule ]).
	
	testClass rename: 'TotoTestCase'.

	self assert: (testClass critiques noneSatisfy: [ :critic | critic rule class = ReTestClassNameShouldEndWithTestRule ]).

	"test class name not endind with 'Test'"
	testClass rename: 'Toto'.

	self assert: (testClass critiques anySatisfy: [ :critic | critic rule class = ReTestClassNameShouldEndWithTestRule ])
]
