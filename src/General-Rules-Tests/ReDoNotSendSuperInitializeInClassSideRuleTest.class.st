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
    critiques := (ReCriticEngine nodeCritiquesOf: method)
    select: [ :each |
        each rule class = ReDoNotSendSuperInitializeInClassSideRule ].
    self assert: critiques size equals: 1.
    rewritten := critiques anyOne change source.
    self assert: (rewritten includesSubstring: 'self name').
    self deny: (rewritten includes: $.)
]
