"
I check if no global in the `model` is named the same as `name`
"
Class {
	#name : 'RBIsNotGlobal',
	#superclass : 'RBVariableNameCondition',
	#instVars : [
		'model'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBIsNotGlobal class >> name: aString model: aNavigationModel [

	^ (self name: aString)
		  model: aNavigationModel;
		  yourself
]

{ #category : 'checking' }
RBIsNotGlobal >> check [

	^ (model includesGlobal: name asSymbol) not
]

{ #category : 'accessing' }
RBIsNotGlobal >> model: aNavigationModel [
	model := aNavigationModel
]
