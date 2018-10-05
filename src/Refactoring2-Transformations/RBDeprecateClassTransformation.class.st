"
Marks a class as deprecated and suggests the use of the superclass.

Usage:
transformation := (RBDeprecateClassTransformation 
		class: #RBRemoveClassTransformation)
		transform. 
(ChangesBrowser changes: transformation model changes changes) open
"
Class {
	#name : #RBDeprecateClassTransformation,
	#superclass : #RBCompositeTransformation,
	#instVars : [
		'className'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBDeprecateClassTransformation class >> class: aClassName [

	^ self new
		class: aClassName;
		yourself
]

{ #category : #api }
RBDeprecateClassTransformation class >> model: aRBModel class: aClassName [

	^ self new
		model: aRBModel;
		class: aClassName;
		yourself
]

{ #category : #executing }
RBDeprecateClassTransformation >> buildTransformations [

	^ transformations ifNil: [ 
		transformations := OrderedCollection
			with: (RBAddMethodTransformation
					sourceCode: (String streamContents: [ :code | code
						nextPutAll: 'new';
						nextPutAll: String cr;
						nextPutAll: String tab;
						nextPutAll: 'self deprecated: ''Use superclass '' on: ''4 May 2016'' in: #Pharo60.';
						nextPutAll: String cr;
						nextPutAll: String tab;
						nextPutAll: '^ super new' ])
					in: (className, ' class') asSymbol
					withProtocol: 'deprecation')
			with: (RBAddMethodTransformation
					sourceCode: (String streamContents: [ :code | code 
						nextPutAll: 'deprecated';
						nextPutAll: String cr;
						nextPutAll: String tab;
						nextPutAll: '^ true' ])
					in: (className, ' class') asSymbol
					withProtocol: 'deprecation')
			with: (RBAddMethodTransformation
					sourceCode: (String streamContents: [ :code | code
						nextPutAll: 'systemIcon';
						nextPutAll: String cr;
						nextPutAll: String tab;
						nextPutAll: '^ Smalltalk ui icons iconNamed: #packageDelete' ])
					in: (className, ' class') asSymbol
					withProtocol: 'deprecation')
			with: (RBAddClassCommentTransformation
					 comment: 'Deprecated!!! Use superclass'
					 in: className) ]
]

{ #category : #api }
RBDeprecateClassTransformation >> class: aClassName [

	className := aClassName
]

{ #category : #printing }
RBDeprecateClassTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' class: '.
	className storeOn: aStream.
	aStream nextPut: $)
]
