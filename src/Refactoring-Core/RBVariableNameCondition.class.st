"
I am an abstract class whose subclasses define checks for valid variables (instance and class vars)
"
Class {
	#name : 'RBVariableNameCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'name'
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
RBVariableNameCondition >> name: aString [
	name := aString
]
