"
I am a transformation for deprecate a method.

My preconditions verify, that the old selector exists (in  the current namespace) and that the new selector is a valid selector

The transformation will add the call to the #deprecated:on:in: method.

Example
---------------

Script:
```
	(RBDeprecateMethodTransformation 
		deprecateMethod: #called:on: 
		in: RBRefactoryTestDataApp 
		using: #callFoo) asRefactoring; execute
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
	#name : #RBDeprecateMethodTransformation,
	#superclass : #RBCompositeMethodTransformation,
	#instVars : [
		#newSelector
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #api }
RBDeprecateMethodTransformation class >> deprecateMethod: aSelector in: aClass using: newSelector [
	^ self new
		deprecateMethod: aSelector
		in: aClass
		using: newSelector;
		yourself
]

{ #category : #api }
RBDeprecateMethodTransformation class >> model: aRBSmalltalk deprecateMethod: aSelector in: aClass using: newSelector [
	^ self new
		model: aRBSmalltalk;
		deprecateMethod: aSelector
		in: aClass
		using: newSelector;
		yourself
]

{ #category : #executing }
RBDeprecateMethodTransformation >> buildTransformations [
	| node parseTree method start |

	method := class methodFor: selector.
	parseTree := method parseTree.
	start := parseTree body statements first start.
	node := self parserClass parseExpression: self callDeprecationMethod.
	^ OrderedCollection new add: (RBAddSubtreeTransformation
		interval: (29 to: 29)
		with: self callDeprecationMethod
		from: selector
		in: class);
		yourself
]

{ #category : #accessing }
RBDeprecateMethodTransformation >> callDeprecationMethod [

	^ 'self
		deprecated: ''Use ' , newSelector storeString , ' instead''
		on: 	''' , Date today asString , '''
		in: ''' , SystemVersion current imageVersionString , ''''
]

{ #category : #api }
RBDeprecateMethodTransformation >> deprecateMethod: aSelector in: aClass using: newSel [
	selector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self model classObjectFor: aClass
]

{ #category : #preconditions }
RBDeprecateMethodTransformation >> preconditions [

	^ (RBCondition definesSelector: selector in: class)
		& (RBCondition canUnderstand: newSelector in: class)
		& (RBCondition
							withBlock: [selector numArgs = newSelector numArgs or: [ newSelector numArgs = 0 ]]
							errorString: 'The new selectors should have the same number of arguments as the old one, or not have them at all')
]

{ #category : #printing }
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
