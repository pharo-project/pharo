"
I am a refactoring that safely removes a group of methods from a class.

My preconditions verify that methods to be removed are not referenced anywhere. However I'm aware that methods to be removed can reference other methods to be removed.


"
Class {
	#name : 'RBRemoveMethodsRefactoring',
	#superclass : 'RBCompositeRefactoring',
	#instVars : [
		'selectors',
		'class'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRemoveMethodsRefactoring class >> model: aRBNamespace selectors: aSelectorsCollection from: aClass [

	^ self new
		model: aRBNamespace;
		selectors: aSelectorsCollection 
		from: aClass
]

{ #category : 'instance creation' }
RBRemoveMethodsRefactoring class >> selectors: aSelectorsCollection from: aClass [

	^ self new
		selectors: aSelectorsCollection 
		from: aClass
]

{ #category : 'preconditions' }
RBRemoveMethodsRefactoring >> applicabilityPreconditions [ 

	^ selectors
			inject: self trueCondition
			into: [ :cond :sel | cond & (RBCondition definesSelector: sel in: class) ]
]

{ #category : 'preconditions' }
RBRemoveMethodsRefactoring >> breakingChangePreconditions [

	refactorings do: [ :ref | ref checkSuperMethods ].

	^ RBCondition
		  withBlock: [ self senders isEmpty ]
		  errorString:
		  'Some of the methods have senders outside of the methods being removed'
]

{ #category : 'removing' }
RBRemoveMethodsRefactoring >> removeMethodChanges [

	refactorings do: [ :ref | ref privateTransform ].
	^ self changes
]

{ #category : 'accessing' }
RBRemoveMethodsRefactoring >> selectors [
	
	^ selectors 
]

{ #category : 'accessing' }
RBRemoveMethodsRefactoring >> selectors: aCollection from: aClass [

	selectors := aCollection.
	class := self model classFor: aClass.
	refactorings := aCollection collect: [ :method |
							RBRemoveMethodRefactoring
								model: model
								selector: method
								from: aClass ]
]

{ #category : 'private' }
RBRemoveMethodsRefactoring >> senders [
	
	^ (refactorings collect: [ :ref | ref sendersExcluding: selectors ]) flattened
]
