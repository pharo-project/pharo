"
I am an abstract class whose subclasses define checks for valid variables (instance and class vars)
"
Class {
	#name : 'ReVariableNameCondition',
	#superclass : 'ReVariableCondition',
	#instVars : [
		'violator'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'utilities' }
ReVariableNameCondition class >> name: aString [

	^ self new name: aString
]

{ #category : 'accessing' }
ReVariableNameCondition >> name: aString [
	variable := aString
]

{ #category : 'displaying' }
ReVariableNameCondition >> violationMessageOn: stream [
	self subclassResponsibility 
]

{ #category : 'accessing' }
ReVariableNameCondition >> violator [
	^ violator
]

{ #category : 'accessing' }
ReVariableNameCondition >> violatorCandidates [

	^ variable
]
