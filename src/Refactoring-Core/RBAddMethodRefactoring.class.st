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
	#name : #RBAddMethodRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'transformation',
		'protocols',
		'source'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBAddMethodRefactoring class >> addMethod: aString toClass: aClass inProtocols: protocolList [
	^ self new
		addMethod: aString
		toClass: aClass
		inProtocols: protocolList
]

{ #category : #'instance creation' }
RBAddMethodRefactoring class >> model: aRBSmalltalk addMethod: aString toClass: aClass inProtocols: protocolList [
	^ self new
		model: aRBSmalltalk;
		addMethod: aString
			toClass: aClass
			inProtocols: protocolList;
		yourself
]

{ #category : #initialization }
RBAddMethodRefactoring >> addMethod: aString toClass: aClass inProtocols: protocolList [
	class := self classObjectFor: aClass.
	source := aString.
	protocols := protocolList.
	transformation := RBAddMethodTransformRefactoring
								model: model
								addMethod: aString
								toClass: aClass
								inProtocols: protocolList 
]

{ #category : #preconditions }
RBAddMethodRefactoring >> preconditions [

	transformation checkPreconditions.
	
	^ (RBCondition canUnderstand: transformation selector in: class) not
]

{ #category : #transforming }
RBAddMethodRefactoring >> privateTransform [

	transformation privateTransform
]

{ #category : #printing }
RBAddMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' addMethod: ''';
		nextPutAll: source;
		nextPutAll: ''' toClass: '.
	class storeOn: aStream.
	aStream nextPutAll: ' inProtocols: '.
	protocols storeOn: aStream.
	aStream nextPut: $)
]
