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
RBDeprecateMethodRefactoring class >> model: aRBSmalltalk deprecateMethod: aSelector in: aClass using: newSelector [
	^ self new
		model: aRBSmalltalk;
		deprecateMethod: aSelector
			in: aClass
			using: newSelector;
		yourself
]

{ #category : #transforming }
RBDeprecateMethodRefactoring >> callDeprecationMethod [

	^ 'self 
		deprecated: ''Use ' , newSelector storeString , ' instead''
		on: 	''' , Date today asString , ''' 
		in: ''' , SystemVersion current imageVersionString , ''''
]

{ #category : #preconditions }
RBDeprecateMethodRefactoring >> deprecateMethod: aSelector in: aClass using: newSel [
	oldSelector := aSelector asSymbol.
	newSelector := newSel asSymbol.
	class := self classObjectFor: aClass.
]

{ #category : #preconditions }
RBDeprecateMethodRefactoring >> preconditions [

	^ (RBCondition definesSelector: oldSelector in: class) 
		& (RBCondition canUnderstand: newSelector in: class)
		& (RBCondition 
							withBlock: [oldSelector numArgs = newSelector numArgs or: [ newSelector numArgs = 0 ]]
							errorString: 'The new selectors should have the same number of arguments as the old one, or not have them at all').
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

{ #category : #transforming }
RBDeprecateMethodRefactoring >> transform [

	| node parseTree method |
	method := class methodFor: oldSelector.
	parseTree := method parseTree.
	node := self parserClass parseExpression: self callDeprecationMethod.
	parseTree body addNodeFirst: node.
	class compile: parseTree newSource classified: method protocols
]
