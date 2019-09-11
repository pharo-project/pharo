"
I am a refactoring for adding methods to a class.

My operation will compile a new method to a class in the specified protocol.

You can create an instance with: 
RBAddMethodRefactoring model:RBNamespace new addMethod:'foo ^ self' toClass:Morph inProtocols:{'test'}.

The method to compile is the full method source (selector, arguments and code).

My precondition verifies that the methods source can be parsed and that the class does not already understands this methods selectors. That means, you can not use this refactoring to add methods for overwriting superclass methods.

"
Class {
	#name : #RBAddMethodRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'protocols',
		'source'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBAddMethodRefactoring class >> addMethod: aString toClass: aClass inProtocols: protocolList [ 
	^self new addMethod: aString
		toClass: aClass
		inProtocols: protocolList
]

{ #category : #'instance creation' }
RBAddMethodRefactoring class >> model: aRBSmalltalk addMethod: aString toClass: aClass inProtocols: protocolList [ 
	^(self new)
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
	protocols := protocolList
]

{ #category : #preconditions }
RBAddMethodRefactoring >> preconditions [
	| selector method |
	method := self parserClass
		parseMethod: source
		onError: [ :string :position | 
			^ RBCondition
				withBlock: [ self refactoringFailure: 'The sources could not be parsed' ] ].
	selector := method selector.
	selector ifNil: [ self refactoringFailure: 'Invalid source.' ].
	^ (RBCondition canUnderstand: selector in: class) not
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

{ #category : #transforming }
RBAddMethodRefactoring >> transform [
	class compile: source classified: protocols
]
