"
I'm a transformation whose focus is class.  
"
Class {
	#name : #RBClassTransformation,
	#superclass : #RBTransformation,
	#instVars : [
		'className',
		'class',
		'environment'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #accessing }
RBClassTransformation >> className [

	^ className ifNil: [ 
		UIManager default
			chooseOrRequestFrom: (self environment allClasses collect: #name)
			title: 'Class name was not defined to perform ', self class baseClass name,
					 '. Please select a class to resume this transformation.' ]
]

{ #category : #accessing }
RBClassTransformation >> className: anObject [

	className := anObject
]

{ #category : #accessing }
RBClassTransformation >> definingClass [

	^ class ifNil: [ 
		class := self model 
					classObjectFor: self className ]
]

{ #category : #accessing }
RBClassTransformation >> environment [
	^ environment
]

{ #category : #accessing }
RBClassTransformation >> environment: anObject [
	environment := anObject
]

{ #category : #initialization }
RBClassTransformation >> initialize [ 
	super initialize.
	environment := Smalltalk globals
]

{ #category : #preconditions }
RBClassTransformation >> parseTreeRewriterClass [
	^ RBParseTreeRewriter
]

{ #category : #executing }
RBClassTransformation >> parserClass [
	^ RBParser
]
