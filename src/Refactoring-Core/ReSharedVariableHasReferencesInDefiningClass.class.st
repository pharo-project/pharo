"
I check if a Shared Variable is referenced from methods in the class that defines it.
"
Class {
	#name : 'ReSharedVariableHasReferencesInDefiningClass',
	#superclass : 'ReVariableCondition',
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReSharedVariableHasReferencesInDefiningClass >> check [

	^ self violators isNotEmpty
]

{ #category : 'initialization' }
ReSharedVariableHasReferencesInDefiningClass >> class: definingClass referencesToSharedVariable: aSharedVarName [
	aClass := definingClass.
	variable := aSharedVarName 
]

{ #category : 'accessing' }
ReSharedVariableHasReferencesInDefiningClass >> violatorCandidates [

	^ aClass selectors, aClass classSide selectors
]

{ #category : 'accessing' }
ReSharedVariableHasReferencesInDefiningClass >> violators [
	| res |
	violators ifNotNil: [ ^ violators ].
	violators := OrderedCollection new.
	res := aClass whichMethodsReferToSharedVariable: variable.
	res isNotEmpty	
		ifTrue: [ violators addAll: res ].
	res := (aClass classSide whichMethodsReferToSharedVariable: variable).
	res isNotEmpty
		ifTrue: [ violators addAll: res ].
	^ violators
]
