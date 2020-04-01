Class {
	#name : #ReTestClassNameShouldNotEndWithTestsTest,
	#superclass : #ReTestBasedTestCase,
	#category : #'Renraku-Tests'
}

{ #category : #tests }
ReTestClassNameShouldNotEndWithTestsTest >> testBasicCheck [
	self
		assert:
			(testClass critiques
				noneSatisfy: [ :critic | critic rule class = ReTestClassNameShouldNotEndWithTests ]).


	"test class name endind with 'Tests'"
	testClass rename: 'RenrakuProgrammaticallyCreatedClassTests'.
	self
		assert:
			(testClass critiques
				anySatisfy: [ :critic | critic rule class = ReTestClassNameShouldNotEndWithTests ])
]
