"
I am a refactoring that safely removes a group of methods from a class.

My preconditions verify that methods to be removed are not referenced anywhere. However I'm aware that methods to be removed can reference other methods to be removed.


"
Class {
	#name : #RBRemoveMethodsRefactoring,
	#superclass : #RBCompositeRefactoring,
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRemoveMethodsRefactoring class >> model: aRBNamespace methods: aSelectorsCollection from: aClass [

	^ self new
		model: aRBNamespace;
		methods: aSelectorsCollection 
		from: aClass
]

{ #category : #preconditions }
RBRemoveMethodsRefactoring >> breakingChangePreconditions [

	refactorings do: [ :ref | ref checkSuperMethods ].

	^ RBCondition
		  withBlock: [ self senders isEmpty ]
		  errorString:
		  'Some of the methods have senders outside of the methods being removed'
]

{ #category : #accessing }
RBRemoveMethodsRefactoring >> methods: aCollection from: aClass [

	refactorings := aCollection collect: [ :method |
							RBRemoveMethodRefactoring
								model: model
								selector: method
								from: aClass ]
]

{ #category : #removing }
RBRemoveMethodsRefactoring >> removeMethodChanges [

	refactorings do: [ :ref | ref privateTransform ].
	^ self changes
]

{ #category : #accessing }
RBRemoveMethodsRefactoring >> selectors [
	
	^ refactorings collect: [ :ref | ref selector ]
]

{ #category : #accessing }
RBRemoveMethodsRefactoring >> selectors: aCollection [

	refactorings := aCollection collect: [ :selector |
							RBRemoveMethodTransformation
								model: model
								selector: selector
								from: selector origin ]
]

{ #category : #private }
RBRemoveMethodsRefactoring >> senders [
	
	| selectors |
	selectors := refactorings collect: [ :ref | ref selector ].
	^ (refactorings collect: [ :ref | ref sendersExcluding: selectors ]) flattened
]
