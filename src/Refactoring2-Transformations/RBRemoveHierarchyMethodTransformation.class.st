Class {
	#name : #RBRemoveHierarchyMethodTransformation,
	#superclass : #RBCompositeMethodTransformation,
	#instVars : [
		#selectors
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #removing }
RBRemoveHierarchyMethodTransformation class >> removeMethods: selectorCollection from: aClass [
	^ self new
		removeMethods: selectorCollection from: aClass
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
		on: RBRefactoringError
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

{ #category : #preconditions }
RBRemoveHierarchyMethodTransformation >> preconditions [

	^ selectors inject: self emptyCondition into: [ :cond :aSelector |
		  cond & (RBCondition definesSelector: aSelector in: class) ]
]

{ #category : #removing }
RBRemoveHierarchyMethodTransformation >> removeMethods: selectorCollection from: aClass [
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
