"
I am a refactoring for adding methods to a class.

My operation will compile a new method to a class in the specified protocol.

You can create an instance with: 

```
RBAddMethodRefactoring 
	model: RBNamespace new 
	addMethod:'foo ^ self' 
	toClass:Morph
	inProtocols:{'test'}.
```

The method to compile is the full method source (selector, arguments and code).

I am composed of `RBAddMethodTransformRefactoring`.

My precondition verifies that the class does not already understands this methods selectors. That means, you can not use this refactoring to add methods for overwriting superclass methods.
My preconditions also verify that `RBAddMethodTransformRefactoring` preconditions are met.

"
Class {
	#name : 'RBAddMethodRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'transformation',
		'source',
		'protocol'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBAddMethodRefactoring class >> model: aRBSmalltalk sourceCode: aString in: aClass withProtocol: aProtocol [
	^ self new
		model: aRBSmalltalk;
		sourceCode: aString
			in: aClass
			withProtocol: aProtocol;
		yourself
]

{ #category : 'instance creation' }
RBAddMethodRefactoring class >> sourceCode: aString in: aClass withProtocol: aProtocol [
	^ self new
		sourceCode: aString
		in: aClass
		withProtocol: aProtocol
]

{ #category : 'preconditions' }
RBAddMethodRefactoring >> applicabilityPreconditions [ 

	^ transformation applicabilityPreconditions
]

{ #category : 'preconditions' }
RBAddMethodRefactoring >> breakingChangePreconditions [

	^ { (RBCondition canUnderstand: transformation selector in: class) not }
]

{ #category : 'preconditions' }
RBAddMethodRefactoring >> preconditions [ 

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBAddMethodRefactoring >> prepareForExecution [ 

	transformation prepareForExecution 
]

{ #category : 'transforming' }
RBAddMethodRefactoring >> privateTransform [

	transformation privateTransform
]

{ #category : 'initialization' }
RBAddMethodRefactoring >> sourceCode: aString in: aClass withProtocol: aProtocol [
	class := self classObjectFor: aClass.
	source := aString.
	protocol := aProtocol.
	transformation := RBAddMethodTransformation
								model: model
								sourceCode: aString
								in: aClass
								withProtocol: aProtocol 
]

{ #category : 'storing' }
RBAddMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' sourceCode: ''';
		nextPutAll: source;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream nextPutAll: ' withProtocol: '.
	protocol storeOn: aStream.
	aStream nextPut: $)
]
