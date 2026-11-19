Class {
	#name : 'ReDoNotSendSuperInitializeInClassSideRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'tests' }
ReDoNotSendSuperInitializeInClassSideRuleTest >> testAutoFixRemovesFollowingPeriod [

    | method critiques rewritten |
    method := self
        generateMethod: 'initialize
    super initialize.
    self name'
        classified: 'class initialization'
        in: self class class.
    critiques := self myCritiquesOnMethod: method.
    self assert: critiques size equals: 1.
    critiques first sourceAnchor initializeEnitity: method.
    rewritten := critiques first change source.
    self assert: (rewritten includesSubstring: 'self name').
    self deny: (rewritten includes: $.)
]
