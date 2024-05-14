"
I am a refactoring that safely removes a group of methods from a class.

My preconditions verify that methods to be removed are not referenced anywhere. However I'm aware that methods to be removed can reference other methods to be removed.


"
Class {
	#name : 'ReRemoveMethodsRefactoring',
	#superclass : 'ReCompositeRefactoring',
	#instVars : [
		'classSelectorMapping',
		'allSelectors'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
ReRemoveMethodsRefactoring class >> classSelectorMapping: classMethodMappingDict [

	^ self new
		  classSelectorMapping: classMethodMappingDict
]

{ #category : 'instance creation' }
ReRemoveMethodsRefactoring class >> model: model classSelectorMapping: classMethodMappingDict [

	^ self new
		  model: model;
		  classSelectorMapping: classMethodMappingDict
]

{ #category : 'instance creation' }
ReRemoveMethodsRefactoring class >> model: aRBNamespace selectors: aSelectorsCollection from: aClass [

	^ self new
		model: aRBNamespace;
		selectors: aSelectorsCollection 
		from: aClass
]

{ #category : 'instance creation' }
ReRemoveMethodsRefactoring class >> selectors: aSelectorsCollection from: aClass [

	^ self new
		selectors: aSelectorsCollection 
		from: aClass
]

{ #category : 'accessing' }
ReRemoveMethodsRefactoring >> allSelectors [
	
	^ allSelectors ifNil: [ allSelectors := classSelectorMapping values flattened ]
]

{ #category : 'preconditions' }
ReRemoveMethodsRefactoring >> applicabilityPreconditions [
	"Return a list of preconditions"

	| preconditions |
	preconditions := OrderedCollection new.
	classSelectorMapping keysAndValuesDo: [ :class :selectorsList |
		preconditions add: (ReDefinesSelectorsCondition new
				 definesSelectors: selectorsList
				 in: class) ].
	^ preconditions
]

{ #category : 'preconditions' }
ReRemoveMethodsRefactoring >> breakingChangePreconditions [

	refactorings do: [ :ref | ref checkSuperMethods ].

	^ { self preconditionHaveNoSenders }
]

{ #category : 'preconditions' }
ReRemoveMethodsRefactoring >> checkSuperMethods [
	
	refactorings do: [ :ref | ref checkSuperMethods ]
]

{ #category : 'initialization' }
ReRemoveMethodsRefactoring >> classSelectorMapping: classSelectorMappingList [

	classSelectorMapping := Dictionary new.
	refactorings := OrderedCollection new.
	classSelectorMappingList do: [ :aClassSelectorPair |
		| aClass existingValue newValue |
		aClass := self model classObjectFor: aClassSelectorPair key.
		existingValue := classSelectorMapping at: aClass ifAbsent: nil.
		newValue := existingValue
			            ifNil: [ Array with: aClassSelectorPair value ]
			            ifNotNil: [ existingValue copyWith: aClassSelectorPair value ].
		classSelectorMapping at: aClass put: newValue.
		refactorings add: (RBRemoveMethodRefactoring
				 model: model
				 selector: aClassSelectorPair value
				 from: aClass) ]
]

{ #category : 'preconditions' }
ReRemoveMethodsRefactoring >> preconditionHaveNoSenders [

	^ ReMethodsHaveNoSendersCondition new model: self model; classSelectorsMapping: classSelectorMapping
]

{ #category : 'removing' }
ReRemoveMethodsRefactoring >> removeMethodChanges [

	refactorings do: [ :ref | ref privateTransform ].
	^ self changes
]

{ #category : 'accessing' }
ReRemoveMethodsRefactoring >> selectors: aCollection from: aClass [

	self classSelectorMapping: (aCollection collect: [ :selector | aClass -> selector ] ) 
]
