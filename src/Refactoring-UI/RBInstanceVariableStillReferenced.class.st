Class {
	#name : #RBInstanceVariableStillReferenced,
	#superclass : #RBBreakingChangeReport,
	#instVars : [
		'environment',
		'refactoring'
	],
	#category : #'Refactoring-UI'
}

{ #category : #accessing }
RBInstanceVariableStillReferenced >> environment [

	^ environment
]

{ #category : #accessing }
RBInstanceVariableStillReferenced >> refactoring [
	^ refactoring
]

{ #category : #accessing }
RBInstanceVariableStillReferenced >> refactoring: aRefactoring [
	self halt.
	refactoring := aRefactoring.
	environment := RBBrowserEnvironment new instVarRefsTo: aRefactoring ariable first in: aRefactoring aClassName
]

{ #category : #accessing }
RBInstanceVariableStillReferenced >> variable: aVariable inClassName: aClassName [

	environment := RBBrowserEnvironment new instVarRefsTo: aVariable first in: aClassName
]
