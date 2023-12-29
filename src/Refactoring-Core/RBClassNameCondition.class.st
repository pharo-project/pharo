Class {
	#name : 'RBClassNameCondition',
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
RBClassNameCondition class >> classNamed: aString inModel: aModel [ 
	
	^ self new
		model: aModel;
		classNamed: aString; 
		yourself
]

{ #category : 'accessing' }
RBClassNameCondition >> classNamed: aString [ 
	className := aString
]

{ #category : 'initialization' }
RBClassNameCondition >> initialize [

	super initialize.
	violators := OrderedCollection new.
]

{ #category : 'accessing' }
RBClassNameCondition >> model: aModel [ 
	model := aModel
]

{ #category : 'initialization' }
RBClassNameCondition >> violators [ 

	^ violators
]
