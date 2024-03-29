Class {
	#name : 'ReTestClassNameShouldNotEndWithTestsTest',
	#superclass : 'ReTestBasedTestCase',
	#category : 'SUnit-Rules-Tests-Base',
	#package : 'SUnit-Rules-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
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
