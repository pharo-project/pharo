Class {
	#name : 'ReDirectlyDefinesSharedVariableConditionTest',
	#superclass : 'ReClassesConditionTest',
	#category : 'Refactoring-Core-Tests-Conditions',
	#package : 'Refactoring-Core-Tests',
	#tag : 'Conditions'
}

{ #category : 'tests' }
ReDirectlyDefinesSharedVariableConditionTest >> testDirectlyDefinesSharedVariable [

    | myClassAlpha condition |

    myClassAlpha := self model classNamed: #MyClassAlpha.
    condition := ReDirectlyDefinesSharedVariableCondition
        class: myClassAlpha
        sharedVariables: { #SharedVarA }.
    self assert: condition check
]

{ #category : 'tests' }
ReDirectlyDefinesSharedVariableConditionTest >> testDoesNotDefineSharedVariable [

    | myClassAlpha condition |
    myClassAlpha := self model classNamed: #MyClassAlpha.
    condition := ReDirectlyDefinesSharedVariableCondition
        class: myClassAlpha
        sharedVariables: { #SharedVarB }.
    self deny: condition check
]

{ #category : 'tests' }
ReDirectlyDefinesSharedVariableConditionTest >> testInheritedSharedVariableFails [

    | myClassBeta condition |
    myClassBeta := self model classNamed: #MyClassBeta.
    "SharedVarA comes from MyClassAlpha"
    condition := ReDirectlyDefinesSharedVariableCondition
        class: myClassBeta
        sharedVariables: { #SharedVarA }.
    self deny: condition check
]
