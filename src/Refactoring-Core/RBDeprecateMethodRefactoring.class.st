"
I am a refactoring for deprecate a method.

My preconditions verify, that the old selector exists (in  the current namespace) and that the new selector is a valid selector

The refactoring transformation will add the call to the #deprecated:on:in: method 

Example
---------------

Script:
```
	(RBDeprecateMethodRefactoring 
		deprecateMethod: #called:on: 
		in: RBRefactoryTestDataApp 
		using: #callFoo) execute
```

Before refactoring:
```
RBRefactoryTestDataApp >> called: anObject on: aBlock 
	Transcript
		show: anObject printString;
		cr.
	aBlock value
```

After refactoring:
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
	#name : #RBDeprecateMethodRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'oldSelector',
		'newSelector'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'intance creation' }
RBDeprecateMethodRefactoring class >> deprecateMethod: aSelector in: aClass using: newSelector [
	^ self new
		deprecateMethod: aSelector
			in: aClass
			using: newSelector;
		yourself
]

{ #category : #'intance creation' }
RBDeprecateMethodRefactoring class >> model: aRBSmalltalk deprecateMethod: aSelector in: aClass [
	"partial initialized refactoring"
	
	^ self new
		  model: aRBSmalltalk;
		  deprecateMethod: aSelector in: aClass;
		  yourself
]

{ #category : #'intance creation' }
RBDeprecateMethodRefactoring class >> model: aRBSmalltalk deprecateMethod: aSelector in: aClass using: newSelector [
	^ self new
		model: aRBSmalltalk;
		deprecateMethod: aSelector
			in: aClass
			using: newSelector;
		yourself
]

{ #category : #preconditions }
RBDeprecateMethodRefactoring >> applicabilityPreconditions [

	^ RBCondition definesSelector: oldSelector in: class
]

{ #category : #preconditions }
RBDeprecateMethodRefactoring >> breakingChangePreconditions [

	^ (RBCondition canUnderstand: newSelector in: class) & (RBCondition
		   withBlock: [
			   oldSelector numArgs = newSelector numArgs or: [
				   newSelector numArgs = 0 ] ]
		   errorString:
		   'The new selectors should have the same number of arguments as the old one, or not have them at all')
]

{ #category : #transforming }
RBDeprecateMethodRefactoring >> callDeprecationMethod [

	^ 'self
		deprecated: ''Use ' , newSelector storeString , ' instead''
		on: 	''' , Date today asString , '''
		in: ''' , SystemVersion current imageVersionString , ''''
]

{ #category : #initialization }
RBDeprecateMethodRefactoring >> deprecateMethod: aSelector in: aClass [
	"partial initialization"
	
	oldSelector := aSelector asSymbol.
	class := self classObjectFor: aClass
]

{ #category : #initialization }
RBDeprecateMethodRefactoring >> deprecateMethod: aSelector in: aClass using: newSel [

	self deprecateMethod: aSelector in: aClass.
	newSelector := newSel
]

{ #category : #execution }
RBDeprecateMethodRefactoring >> generateChanges [

	self inputValidation check  ifFalse: [
		RBInputValidationFailedError signal:
			self inputValidation errorString ].
	self applicabilityPreconditions check ifFalse: [
		^ RBApplicabilityChecksFailedError signal:
			  self applicabilityPreconditions errorString ].
	self breakingChangePreconditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangePreconditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : #preconditions }
RBDeprecateMethodRefactoring >> inputValidation [

	^ (RBCondition
		   withBlock: [
			   newSelector isEmptyOrNil  or: [
				   newSelector = oldSelector ]]
		   errorString:
		   'The new selectors cannot be an empty string or same as the old selector.') not
]

{ #category : #accessing }
RBDeprecateMethodRefactoring >> newSelector: aString [

	newSelector := aString asSymbol
]

{ #category : #preconditions }
RBDeprecateMethodRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : #transforming }
RBDeprecateMethodRefactoring >> privateTransform [

	| node parseTree method |
	method := class methodFor: oldSelector.
	parseTree := method parseTree.
	node := self parserClass parseExpression: self callDeprecationMethod.
	parseTree body addNodeFirst: node.

	self generateChangesFor:
		(RBAddMethodTransformation
			 sourceCode: parseTree newSource
			 in: class
			 withProtocol: method protocols)
]

{ #category : #printing }
RBDeprecateMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' deprecateMethod: #';
		nextPutAll: oldSelector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' using: #';
		nextPutAll: newSelector.
	aStream nextPut: $)
]
