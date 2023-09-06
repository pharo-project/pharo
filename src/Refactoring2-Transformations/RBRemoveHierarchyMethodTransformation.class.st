Class {
	#name : #RBRemoveHierarchyMethodTransformation,
	#superclass : #RBCompositeMethodTransformation,
	#instVars : [
		'selectors'
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #removing }
RBRemoveHierarchyMethodTransformation class >> removeMethod: selectorCollection from: aClass [
	^ self new
		removeMethod: selectorCollection from: aClass
]

{ #category : #preconditions }
RBRemoveHierarchyMethodTransformation >> applicabilityPreconditions [ 

	^ selectors inject: self trueCondition into: [ :cond :aSelector |
		  cond & (RBCondition definesSelector: aSelector in: class) ]
]

{ #category : #executing }
RBRemoveHierarchyMethodTransformation >> buildTransformations [
	transformations := OrderedCollection new.
	self deleteSelectors.
	^ transformations
]

{ #category : #transforming }
RBRemoveHierarchyMethodTransformation >> delete: aSelector in: aClass [
	 (aClass realClass includesSelector: aSelector) ifTrue: [
		[transformations add: (RBRemoveMethodTransformation
		model: self model
		selector: aSelector
		from: aClass).]
		 "formCompositeRefactoring: (RBRemoveMethodRefactoring
									model: self model
									removeMethods: { aSelector }
									from: aClass) ]"
		on: RBRefactoringWarning
			do: [ :ex | ( self confirm:
				(ex messageText copyReplaceAll: 'Browse references?' with: 'Remove anyway?') )
				ifTrue:[ ex resume ] ] ]
]

{ #category : #executing }
RBRemoveHierarchyMethodTransformation >> deleteSelectors [
	^ selectors do:  [ :aSelector |
		class withAllSubclasses do: [ :aClass |
			self delete: aSelector in: aClass
		 ]
	 ]
]

{ #category : #removing }
RBRemoveHierarchyMethodTransformation >> removeMethod: selectorCollection from: aClass [
	class := self model classObjectFor: aClass.
	selectors := selectorCollection
]

{ #category : #printing }
RBRemoveHierarchyMethodTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' removeMethods: '.
	selectors asArray storeOn: aStream.
	aStream nextPutAll: ' from: '.
	class storeOn: aStream.
	aStream nextPut: $)
]
