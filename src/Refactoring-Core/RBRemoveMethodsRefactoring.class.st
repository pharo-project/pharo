"
I am a refactoring that safely removes a group of methods from a class.

My preconditions verify that methods to be removed are not referenced anywhere. However I'm aware that methods to be removed can reference other methods to be removed.


"
Class {
	#name : 'RBRemoveMethodsRefactoring',
	#superclass : 'RBCompositeRefactoring',
	#instVars : [
		'classSelectorMapping',
		'allSelectors'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRemoveMethodsRefactoring class >> classSelectorMapping: classMethodMappingDict [

	^ self new
		  classSelectorMapping: classMethodMappingDict
]

{ #category : 'instance creation' }
RBRemoveMethodsRefactoring class >> model: model classSelectorMapping: classMethodMappingDict [

	^ self new
		  model: model;
		  classSelectorMapping: classMethodMappingDict
]

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

{ #category : 'accessing' }
RBRemoveMethodsRefactoring >> allSelectors [
	
	^ allSelectors ifNil: [ allSelectors := classSelectorMapping values flattened ]
]

{ #category : 'preconditions' }
RBRemoveMethodsRefactoring >> applicabilityPreconditions [

	| preconditions |
	preconditions := OrderedCollection new.
	classSelectorMapping
		keysAndValuesDo: [ :class :selectorsList |
			preconditions add:
					(RBDefinesSelectorsCondition new
						 definesSelectors: selectorsList
						 in: class) ].
	^ preconditions
]

{ #category : 'preconditions' }
RBRemoveMethodsRefactoring >> breakingChangePreconditions [

	refactorings do: [ :ref | ref checkSuperMethods ].

	^ RBMethodsHaveNoSendersCondition new model: self model; classSelectorsMapping: classSelectorMapping
]

{ #category : 'initialization' }
RBRemoveMethodsRefactoring >> classSelectorMapping: classSelectorMappingList [

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

{ #category : 'removing' }
RBRemoveMethodsRefactoring >> removeMethodChanges [

	refactorings do: [ :ref | ref privateTransform ].
	^ self changes
]

{ #category : 'accessing' }
RBRemoveMethodsRefactoring >> selectors: aCollection from: aClass [

	self classSelectorMapping: (aCollection collect: [ :selector | aClass -> selector ] ) 
]
