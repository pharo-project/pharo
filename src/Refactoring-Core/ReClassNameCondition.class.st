Class {
	#name : 'ReClassNameCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'violators',
		'model',
		'className'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
ReClassNameCondition class >> classNamed: aString inModel: aModel [ 
	
	^ self new
		model: aModel;
		classNamed: aString; 
		yourself
]

{ #category : 'accessing' }
ReClassNameCondition >> classNamed: aString [ 
	className := aString
]

{ #category : 'initialization' }
ReClassNameCondition >> initialize [

	super initialize.
	violators := OrderedCollection new.
]

{ #category : 'accessing' }
ReClassNameCondition >> model: aModel [ 
	model := aModel
]

{ #category : 'initialization' }
ReClassNameCondition >> violators [ 

	^ violators
]
