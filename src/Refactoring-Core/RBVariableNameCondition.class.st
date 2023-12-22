"
I am an abstract class whose subclasses define checks for valid variables (instance and class vars)
"
Class {
	#name : 'RBVariableNameCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'name',
		'violator'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'utilities' }
RBVariableNameCondition class >> name: aString [

	^ self new name: aString
]

{ #category : 'utilities' }
RBVariableNameCondition class >> reservedNames [
	^#('self' 'true' 'false' 'nil' 'thisContext' 'super')
]

{ #category : 'accessing' }
RBVariableNameCondition >> errorString [

	^ String streamContents: [ :stream |
		self violationMessageOn: stream
		]
]

{ #category : 'accessing' }
RBVariableNameCondition >> name: aString [
	name := aString
]

{ #category : 'displaying' }
RBVariableNameCondition >> violationMessageOn: stream [
	self subclassResponsibility 
]

{ #category : 'accessing' }
RBVariableNameCondition >> violator [
	^ violator
]
