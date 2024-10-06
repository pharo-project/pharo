"
I am a refactoring condition that checks if a method to be pulled up has duplicated in any target superclass subclasses. A duplicated method has the same body and signature as the original method
"
Class {
	#name : 'ReMethodsHaveNoDuplicatesCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'model',
		'targetSuperclass',
		'selectors',
		'class'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'choices' }
ReMethodsHaveNoDuplicatesCondition class >> strategyChoiceClass [
	^ ReRemoveDuplicateMethodsChoice 
]

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesCondition >> class: aRBClass [
	class := aRBClass
]

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesCondition >> duplicatesOf: aSelector [

	| tree siblings duplicates |
	duplicates := OrderedCollection new.
	tree := class parseTreeForSelector: aSelector.
	siblings := targetSuperclass allSubclasses reject: [ :each | each = class ].
	
	siblings do: [ :each |
		((each directlyDefinesMethod: aSelector) and: [
			 (tree
				  equalTo: (each parseTreeForSelector: aSelector)
				  exceptForVariables: #(  )) ]) 
					ifTrue: [ 	duplicates add: { aSelector . each } ] 
				]. "`each` is the class where the duplicate method is defined"
		^ duplicates
]

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesCondition >> model: aRBNamespace [

	model := aRBNamespace 
]

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesCondition >> selectors: aCollection [ 
	selectors := aCollection
]

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesCondition >> siblingsOfSource [

	^ targetSuperclass allSubclasses reject: [ :each | each = class ].
]

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesCondition >> targetSuperclass: aRBClass [ 
	targetSuperclass := aRBClass
]

{ #category : 'displaying' }
ReMethodsHaveNoDuplicatesCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		| selector definingClass |
		selector := violator at: 1.
		definingClass := violator at: 2.
		aStream
			nextPutAll: 'The method ';
			nextPutAll: definingClass name;
			nextPutAll:'>>';
			nextPutAll: selector;
			nextPutAll: ' is a duplicate of the pulled up method';
			cr]
]

{ #category : 'accessing' }
ReMethodsHaveNoDuplicatesCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	violators := OrderedCollection new.
	violators addAll: (selectors flatCollect: [ :aSelector | self duplicatesOf: aSelector ]).
	^ violators

]

{ #category : 'enumerating' }
ReMethodsHaveNoDuplicatesCondition >> withViolatorsDo: aBlockClosure [

	self violators do: [ :violator |
		| aSelector aClass |
		aSelector := violator at: 1.
		aClass := violator at: 2.
		aBlockClosure value: aSelector value: aClass ]
]
