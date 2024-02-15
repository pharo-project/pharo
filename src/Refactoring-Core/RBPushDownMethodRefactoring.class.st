"
I am a refactoring for moving a method down to all direct subclasses.

My preconditions verify that this method isn't refered  as a super send in the subclass. And the class defining this method is abstract or not referenced anywhere.


"
Class {
	#name : 'RBPushDownMethodRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selectors',
		'classes'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBPushDownMethodRefactoring class >> model: anEnvironment pushDown: selectorCollection from: aClassName [
	^ self new
		model: anEnvironment;
		pushDown: selectorCollection from: aClassName;
		yourself
]

{ #category : 'instance creation' }
RBPushDownMethodRefactoring class >> model: anEnvironment pushDown: selectorCollection from: aClass in: classes [
	^ self new
		model: anEnvironment;
		pushDown: selectorCollection from: aClass in: classes;
		yourself
]

{ #category : 'instance creation' }
RBPushDownMethodRefactoring class >> pushDown: selectorCollection from: aClass [
	^ self new
		pushDown: selectorCollection from: aClass
]

{ #category : 'instance creation' }
RBPushDownMethodRefactoring class >> pushDown: selectorCollection from: aClass in: classes [
	^ self new
		pushDown: selectorCollection from: aClass in: classes
]

{ #category : 'transforming' }
RBPushDownMethodRefactoring >> allClasses [

	^ classes ifNil: [ class subclasses ]
]

{ #category : 'preconditions' }
RBPushDownMethodRefactoring >> applicabilityPreconditions [
	"Check that all selectors are defined in `class`"

	^ selectors collect: [ :selector | RBCondition definesSelector: selector in: class ]
]

{ #category : 'preconditions' }
RBPushDownMethodRefactoring >> breakingChangePreconditions [
	"Check that that none of the subclasses of `class` is doing a supercall to any of the selectors
	that will be pushed down.
	
	Also, to ensure that an instance of the class is not sent a message which is pushed down,  forces that 
	we can only push down methods from abstract class. 
	This should be controlled via a flag on the ui.
	"

	| conditions |
	conditions := OrderedCollection with: (RBCondition isAbstractClass: class).
	conditions addAll: (selectors collect: [ :selector |
		              (RBCondition
			              subclassesOf: class
			              isDoingASuperSendFor: selector) not ]).
	^ conditions
]

{ #category : 'transforming' }
RBPushDownMethodRefactoring >> classes: aCollection [
	classes := aCollection collect: [ :cls |
		self classObjectFor: cls.
	]
]

{ #category : 'executing' }
RBPushDownMethodRefactoring >> generateChanges [
	"Prepare, check the preconditions, and perform the actual transformation (i.e., creating a list of changes that can be accessible using the changes message). This method should not be invoked from drivers since drivers usually check in finer grained mode the preconditions - and that this stage they already checked the preconditions."
	
	self prepareForExecution.

	self applicabilityPreconditions check ifFalse: [
		RBApplicabilityChecksFailedError signal:
			self applicabilityPreconditions errorString ].
	self breakingChangePreconditions check ifFalse: [
		RBBreakingChangeChecksFailedWarning signal:
			self breakingChangePreconditions errorString ].

	self privateTransform.
	^ self changes
]

{ #category : 'preconditions' }
RBPushDownMethodRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions 
]

{ #category : 'transforming' }
RBPushDownMethodRefactoring >> privateTransform [

	selectors do: [ :each | self pushDown: each ].
	selectors do: [ :each |
		self generateChangesFor:
			(RBRemoveMethodTransformation selector: each from: class) ]
]

{ #category : 'transforming' }
RBPushDownMethodRefactoring >> pushDown: aSelector [

	| code protocols refactoring |
	code := class sourceCodeFor: aSelector.
	protocols := class protocolsFor: aSelector.
	refactoring := RBExpandReferencedPoolsRefactoring
		               model: self model
		               forMethod: (class parseTreeForSelector: aSelector)
		               fromClass: class
		               toClasses: self allClasses.
	self generateChangesFor: refactoring.
	self allClasses do: [ :each |
		(each directlyDefinesMethod: aSelector) ifFalse: [
			self generateChangesFor:
				(RBAddMethodTransformation
					sourceCode: code
					in: each
					withProtocol: protocols) ] ]
]

{ #category : 'initialization' }
RBPushDownMethodRefactoring >> pushDown: selectorCollection from: aClassName [
	class := self model classNamed: aClassName.
	selectors := selectorCollection
]

{ #category : 'initialization' }
RBPushDownMethodRefactoring >> pushDown: selectorCollection from: aClass in: aCollection [
	class := self classObjectFor: aClass.
	selectors := selectorCollection.
	self classes: aCollection
]

{ #category : 'transforming' }
RBPushDownMethodRefactoring >> selectors [
	^ selectors
]

{ #category : 'transforming' }
RBPushDownMethodRefactoring >> selectors: aList [
	selectors := aList
]

{ #category : 'storing' }
RBPushDownMethodRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' pushDown: '.
	selectors asArray storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'preconditions' }
RBPushDownMethodRefactoring >> targetClass [
	^ class
]
