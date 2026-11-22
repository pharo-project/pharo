Class {
	#name : 'ReUnaryAccessingMethodWithoutReturnRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'tests' }
ReUnaryAccessingMethodWithoutReturnRuleTest >> testIgnoresInitialize [
    | method |
    method := self
        generateMethod: 'initialize
    super initialize.
    temporaryVariable := 0'
        classified: 'accessing - good'
        in: RBSmalllintTestObject.
    self assertEmpty: (self myCritiquesOnMethod: method)
]
