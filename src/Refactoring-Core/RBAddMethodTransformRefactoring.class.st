"
I am a transformation for adding methods to a class.

My operation will compile a new method to a class in the specified protocol.

You can create an instance with: 

```
RBAddMethodTransformRefactoring 
	model: RBNamespace new 
	addMethod:'foo ^ self' 
	toClass:Morph
	inProtocols:{'test'}.
```

The method to compile is the full method source (selector, arguments and code).

My precondition verifies that the methods source can be parsed.
"
Class {
	#name : #RBAddMethodTransformRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'protocols',
		'source',
		'method'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #adding }
RBAddMethodTransformRefactoring class >> addMethod: aString toClass: aClass inProtocols: protocolList [
	^ self new
		addMethod: aString
		toClass: aClass
		inProtocols: protocolList
]

{ #category : #'instance creation' }
RBAddMethodTransformRefactoring class >> model: aRBSmalltalk addMethod: aString toClass: aClass inProtocols: protocolList [
	^ self new
		model: aRBSmalltalk;
		addMethod: aString
			toClass: aClass
			inProtocols: protocolList;
		yourself
]

{ #category : #adding }
RBAddMethodTransformRefactoring >> addMethod: aString toClass: aClass inProtocols: protocolList [
	class := self classObjectFor: aClass.
	source := aString.
	protocols := protocolList
]

{ #category : #accessing }
RBAddMethodTransformRefactoring >> method [

	^ method
]

{ #category : #preconditions }
RBAddMethodTransformRefactoring >> preconditions [

	method := self parserClass
		          parseMethod: source
		          onError: [ :string :position |
			          ^ RBCondition withBlock: [
				            self refactoringError:
					            'The sources could not be parsed' ] ].
	method selector ifNil: [
		^ RBCondition withBlock: [ self refactoringError: 'Invalid source.' ] ].
	^ self trueCondition
]

{ #category : #accessing }
RBAddMethodTransformRefactoring >> selector [

	^ method selector
]

{ #category : #printing }
RBAddMethodTransformRefactoring >> storeOn: aStream [
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

{ #category : #transforming }
RBAddMethodTransformRefactoring >> transform [
	class compile: source classified: protocols
]
