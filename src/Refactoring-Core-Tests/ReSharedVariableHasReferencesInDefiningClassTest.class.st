Class {
	#name : 'ReSharedVariableHasReferencesInDefiningClassTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReSharedVariableHasReferencesInDefiningClassTest >> testDefinedSharedVariableHasNoReferences [

    | condition |
    condition := ReSharedVariableHasReferencesInDefiningClass new
        class: (self model classNamed: #MyClassAlpha)
        referencesToSharedVariable: #SharedVarA.
    self deny: condition check.
    self assert: condition violators isEmpty
]

{ #category : 'tests' }
ReSharedVariableHasReferencesInDefiningClassTest >> testDefinedSharedVariableHasReferences [

    | condition |
    condition := ReSharedVariableHasReferencesInDefiningClass new
        class: (self model classNamed: #MyClassBeta)
        referencesToSharedVariable: #SharedVarB.
    self assert: condition check.
    self assert: condition violators isNotEmpty
]
