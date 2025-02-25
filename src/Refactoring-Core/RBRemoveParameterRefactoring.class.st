"
I am a refactoring for removing (unused) arguments.

My preconditions verify that the argument to be removed is not referenced by the methods and that the new method name isn't alread used.
Any sender of the prior selector will be changed to the new.

If the method contains more than one argument, I request the user to choose one of the arguments.
"
Class {
	#name : 'RBRemoveParameterRefactoring',
	#superclass : 'RBParameterRemovalRefactoring',
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRemoveParameterRefactoring class >> model: aRBSmalltalk removeParameter: aString in: aClass selector: aSelector [
	^ self new
		model: aRBSmalltalk;
		removeParameter: aString
			in: aClass
			selector: aSelector;
		yourself
]

{ #category : 'instance creation' }
RBRemoveParameterRefactoring class >> removeParameter: aString in: aClass selector: aSelector [
	^ self new
		removeParameter: aString
		in: aClass
		selector: aSelector
]

{ #category : 'preconditions' }
RBRemoveParameterRefactoring >> applicabilityPreconditions [

	| imps |
	imps := self model allImplementorsOf: oldSelector.
	self getNewSelector.
	^ super applicabilityPreconditions , (imps collect:
			[ :each |
				(RBCondition withBlock: [(self hasReferencesToTemporaryIn: each) not]
						errorString: 'This argument is still referenced in at least one implementor!!')]) asArray				
]

{ #category : 'transforming' }
RBRemoveParameterRefactoring >> hasReferencesToTemporaryIn: each [

	| tree |

	tree := each parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringError: 'Cannot parse sources.' ].
	^ tree references: ( tree argumentNames at: parameterIndex )
]

{ #category : 'accessing' }
RBRemoveParameterRefactoring >> newSelector: aSelector [
	newSelector := aSelector
]

{ #category : 'initialization' }
RBRemoveParameterRefactoring >> removeParameter: aString in: aClass selector: aSelector [
	oldSelector := aSelector.
	class := self classObjectFor: aClass.
	argument := aString
]

{ #category : 'storing' }
RBRemoveParameterRefactoring >> storeOn: aStream [

	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream
		nextPutAll: ' removeParameter: ''';
		nextPutAll: argument;
		nextPutAll: ''' in: '.
	aStream nextPutAll: class name.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: oldSelector.
	aStream nextPut: $)
]
