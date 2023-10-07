Class {
	#name : 'ReTestClassNotInPackageWithTestEndingNameTest',
	#superclass : 'ReTestBasedTestCase',
	#category : 'SUnit-Rules-Tests-Base',
	#package : 'SUnit-Rules-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
ReTestClassNotInPackageWithTestEndingNameTest >> testBasicCheck [
	
	"test in wrong package raises critic"
	self
		assert: (testClass critiques anySatisfy: [ :critic | critic rule class = ReTestClassNotInPackageWithTestEndingNameRule ]).
	
	"move to correct package"
	validTestPackage addClass: testClass.
	
	self
		assert: (testClass critiques noneSatisfy: [ :critic | critic rule class = ReTestClassNotInPackageWithTestEndingNameRule ])
]
