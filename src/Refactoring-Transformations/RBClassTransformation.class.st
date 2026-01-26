"
I'm a transformation whose focus is class.  
"
Class {
	#name : 'RBClassTransformation',
	#superclass : 'ReTransformation',
	#instVars : [
		'className',
		'class',
		'environment'
	],
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'accessing' }
RBClassTransformation class >> className: aString [

	^ self new
		  className: aString;
		  yourself
]

{ #category : 'accessing' }
RBClassTransformation class >> model: aRBNamespace className: aString [

	^ self new
		  model: aRBNamespace;
		  className: aString;
		  yourself
]

{ #category : 'accessing' }
RBClassTransformation >> className [

	^ className 
]

{ #category : 'accessing' }
RBClassTransformation >> className: anObject [

	className := anObject
]

{ #category : 'accessing' }
RBClassTransformation >> definingClass [

	^ class ifNil: [ class := self model classObjectFor: self className ]
]

{ #category : 'accessing' }
RBClassTransformation >> environment [
	^ environment
]

{ #category : 'accessing' }
RBClassTransformation >> environment: anObject [
	environment := anObject
]

{ #category : 'initialization' }
RBClassTransformation >> initialize [
	super initialize.
	environment := Smalltalk globals
]
