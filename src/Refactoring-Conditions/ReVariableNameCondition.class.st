"
I am an abstract class whose subclasses define checks for valid variables (instance and class vars)
"
Class {
	#name : 'ReVariableNameCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'name',
		'violator'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'utilities' }
ReVariableNameCondition class >> name: aString [

	^ self new name: aString
]

{ #category : 'accessing' }
ReVariableNameCondition >> name: aString [
	name := aString
]

{ #category : 'displaying' }
ReVariableNameCondition >> violationMessageOn: stream [
	self subclassResponsibility 
]

{ #category : 'accessing' }
ReVariableNameCondition >> violator [
	^ violator
]
