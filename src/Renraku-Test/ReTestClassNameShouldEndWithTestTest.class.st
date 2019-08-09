Class {
	#name : #ReTestClassNameShouldEndWithTestTest,
	#superclass : #ReTestBasedTestCase,
	#category : #'Renraku-Test'
}

{ #category : #tests }
ReTestClassNameShouldEndWithTestTest >> testBasicCheck [
	self assert: (testClass critiques noneSatisfy: [ :critic | critic rule class = ReTestClassNameShouldEndWithTest ]).
	
	testClass rename: 'TotoTestCase'.

	self assert: (testClass critiques noneSatisfy: [ :critic | critic rule class = ReTestClassNameShouldEndWithTest ]).

	"test class name not endind with 'Test'"
	testClass rename: 'Toto'.

	self assert: (testClass critiques anySatisfy: [ :critic | critic rule class = ReTestClassNameShouldEndWithTest ])
]
