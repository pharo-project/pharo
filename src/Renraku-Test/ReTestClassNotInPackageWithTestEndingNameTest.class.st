Class {
	#name : #ReTestClassNotInPackageWithTestEndingNameTest,
	#superclass : #ReTestBasedTestCase,
	#category : #'Renraku-Test'
}

{ #category : #tests }
ReTestClassNotInPackageWithTestEndingNameTest >> testBasicCheck [
	
	"test in wrong package raises critic"
	self
		assert: (testClass critiques anySatisfy: [ :critic | critic rule class = ReTestClassNotInPackageWithTestEndingName ]).
	
	"move to correct package"
	validTestPackage importClass: testClass.
	
	self
		assert: (testClass critiques noneSatisfy: [ :critic | critic rule class = ReTestClassNotInPackageWithTestEndingName ])
]
