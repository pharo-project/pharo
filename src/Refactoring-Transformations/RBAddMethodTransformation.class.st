"
Adds a method in a class. The source code of the method, as well as the protocol in which the method will be categorized, can be specified.

Usage:
```
| transformation |
transformation := (RBAddMethodTransformation
		sourceCode: 'printString1 ^super printString'
		in: RBAddMethodTransformation
		withProtocol: #accessing)
		generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
The source code of the method must be syntactically valid.
"
Class {
	#name : 'RBAddMethodTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'sourceCode',
		'rbMethod',
		'protocol'
	],
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'api' }
RBAddMethodTransformation class >> model: aRBModel sourceCode: aString in: aClass withProtocol: aProtocol [

	^ self new
		model: aRBModel;
		sourceCode: aString
		in: aClass
		withProtocol: aProtocol;
		yourself
]

{ #category : 'api' }
RBAddMethodTransformation class >> sourceCode: aString in: aClass withProtocol: aProtocol [

	^ self new
		sourceCode: aString
		in: aClass
		withProtocol: aProtocol;
		yourself
]

{ #category : 'preconditions' }
RBAddMethodTransformation >> applicabilityPreconditions [

	^ {
		  (RBCondition withBlock: [
			   rbMethod ifNil: [
				   self refactoringError:
					   'The content of this method cannot be parsed.' ].
			   true ]).
		  (RBCondition withBlock: [
			   rbMethod selector ifNil: [
				   self refactoringError: 'The method has no selector.' ].
			   true ]) }
]

{ #category : 'accessing' }
RBAddMethodTransformation >> method [

	^ rbMethod 
]

{ #category : 'transforming' }
RBAddMethodTransformation >> prepareForExecution [

	rbMethod := self parserClass
		            parseMethod: sourceCode
		            onError: [ :string :position |
			            self refactoringError:
				            'The content of this method cannot be parsed.' ]
]

{ #category : 'transforming' }
RBAddMethodTransformation >> privateTransform [

	self definingClass
		compile: sourceCode
		classified: protocol
]

{ #category : 'accessing' }
RBAddMethodTransformation >> selector [

	^ rbMethod selector
]

{ #category : 'api' }
RBAddMethodTransformation >> sourceCode: aString in: aClassName withProtocol: aSymbol [

	self className: aClassName.
	sourceCode := aString.
	protocol := aSymbol
]

{ #category : 'storing' }
RBAddMethodTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' sourceCode: '''.
	sourceCode storeOn: aStream.
	aStream nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' withProtocol: '.
	protocol storeOn: aStream.
	aStream nextPut: $)
]
