"
Marks a class as deprecated and suggests the use of the superclass.

Usage:
transformation := (RBDeprecateClassTransformation 
		class: #RBRemoveClassTransformation)
		transform. 
(ChangesBrowser changes: transformation model changes changes) open
"
Class {
	#name : 'RBDeprecateClassTransformation',
	#superclass : 'RBClassTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBDeprecateClassTransformation class >> class: aClassName [

	^ self new
		class: aClassName;
		yourself
]

{ #category : 'api' }
RBDeprecateClassTransformation class >> model: aRBModel class: aClassName [

	^ self new
		model: aRBModel;
		class: aClassName;
		yourself
]

{ #category : 'api' }
RBDeprecateClassTransformation >> class: aClassName [

	className := aClassName
]

{ #category : 'transforming' }
RBDeprecateClassTransformation >> privateTransform [

	self generateChangesFor: (RBAddMethodTransformation
			 sourceCode: (String streamContents: [ :code |
					  code
						  nextPutAll: 'isDeprecated';
						  nextPutAll: String cr;
						  nextPutAll: String tab;
						  nextPutAll: '^ true' ])
			 in: (className , ' class') asSymbol
			 withProtocol: #deprecation)
]

{ #category : 'printing' }
RBDeprecateClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' class: '.
	className storeOn: aStream.
	aStream nextPut: $)
]
