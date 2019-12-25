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
	validTestPackage moveClass: testClass 
		fromPackage: testPackage 
		toTag: (RPackageTag package: testPackage name: #whatever).
	
	self
		assert: (testClass critiques noneSatisfy: [ :critic | critic rule class = ReTestClassNotInPackageWithTestEndingName ])
]
