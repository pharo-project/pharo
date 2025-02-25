"
I am a transformation for deprecate a method.

My preconditions verify, that the old selector exists (in  the current namespace) and that the new selector is a valid selector

The transformation will add the call to the #deprecated:on:in: method.

Example
---------------

Script:
```
| transformation |
transformation :=  (RBDeprecateMethodTransformation 
		deprecateMethod: #called:on: 
		in: RBClassDataForRefactoringTest 
		using: #callFoo) generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Before
```
RBRefactoryTestDataApp >> called: anObject on: aBlock 
	Transcript
		show: anObject printString;
		cr.
	aBlock value
```

After
```
RBRefactoryTestDataApp >> called: anObject on: aBlock 
	self
		deprecated: 'Use #callFoo instead'
		on: '16 April 2021'
		in: 'Pharo-9.0.0+build.1327.sha.a1d951343f221372d949a21fc1e86d5fc2d2be81 (64 Bit)'.
	Transcript
		show: anObject printString;
		cr.
	aBlock value
```
"
Class {
	#name : 'RBDeprecateMethodTransformation',
	#superclass : 'RBCompositeMethodTransformation',
	#instVars : [
		'newSelector'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'displaying' }
RBDeprecateMethodTransformation class >> basicMenuItemString [

	^ 'Deprecate method'
]

{ #category : 'api' }
RBDeprecateMethodTransformation class >> deprecateMethod: aSelector in: aClass using: newSelector [
	^ self new
		deprecateMethod: aSelector
		in: aClass
		using: newSelector;
		yourself
]

{ #category : 'testing' }
RBDeprecateMethodTransformation class >> isTransformation [

	^ true
]

{ #category : 'api' }
RBDeprecateMethodTransformation class >> model: aRBSmalltalk deprecateMethod: aSelector in: aClass [
	"partial initialized refactoring"
	
	^ self new
		  model: aRBSmalltalk;
		  deprecateMethod: aSelector in: aClass;
		  yourself
]

{ #category : 'api' }
RBDeprecateMethodTransformation class >> model: aRBSmalltalk deprecateMethod: aSelector in: aClass using: newSelector [
	^ self new
		model: aRBSmalltalk;
		deprecateMethod: aSelector
		in: aClass
		using: newSelector;
		yourself
]

{ #category : 'preconditions' }
RBDeprecateMethodTransformation >> applicabilityPreconditions [

	^ {
		  (RBCondition withBlock: [ selector ~= newSelector ]).
		 	(RBCondition isValidSelector: newSelector).
		  (RBCondition definesSelector: selector in: class) }
]

{ #category : 'executing' }
RBDeprecateMethodTransformation >> buildTransformations [

	| node parseTree method |
	method := class methodFor: selector.
	parseTree := method parseTree.
	node := self parserClass parseExpression: self callDeprecationMethod.
	parseTree body addNodeFirst: node.

	^ { (RBAddMethodTransformation
			 sourceCode: parseTree newSource
			 in: class
			 withProtocol: method protocols) } 
]

{ #category : 'accessing' }
RBDeprecateMethodTransformation >> callDeprecationMethod [

	^ 'self
		deprecated: ''Use ' , newSelector asString , ' instead of current one''
		on: 	''' , Date today asString , '''
		in: ''' , SystemVersion current imageVersionString , ''''
]

{ #category : 'api' }
RBDeprecateMethodTransformation >> deprecateMethod: aSelector in: aClass [
	"partial initialization"
	
	selector := aSelector asSymbol.
	class := self model classObjectFor: aClass
]

{ #category : 'api' }
RBDeprecateMethodTransformation >> deprecateMethod: aSelector in: aClass using: newSel [
	self deprecateMethod: aSelector in: aClass.
	newSelector := newSel asSymbol.
]

{ #category : 'api' }
RBDeprecateMethodTransformation >> newSelector: aSymbol [

	newSelector := aSymbol
]

{ #category : 'storing' }
RBDeprecateMethodTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' deprecateMethod: #';
		nextPutAll: selector asString;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' using: #';
		nextPutAll: newSelector asString.
	aStream nextPut: $)
]
