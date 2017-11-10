"
I represent a change in the category of a class or a trait.
"
Class {
	#name : #EpBehaviorCategoryChange,
	#superclass : #EpBehaviorChange,
	#instVars : [
		'oldCategory',
		'newCategory',
		'class'
	],
	#category : #Epicea-Model
}

{ #category : #'instance creation' }
EpBehaviorCategoryChange class >> oldCategory: anOldCategory newCategory: aNewCategory class: aClass [

	^ self basicNew 
		initializeOldCategory: anOldCategory newCategory: aNewCategory class: aClass;
		yourself
]

{ #category : #visitor }
EpBehaviorCategoryChange >> accept: aVisitor [

	^ aVisitor visitBehaviorCategoryChange: self 
]

{ #category : #accessing }
EpBehaviorCategoryChange >> behaviorAffected [
	^ self classAffected
]

{ #category : #accessing }
EpBehaviorCategoryChange >> behaviorAffectedName [
	^ self classAffected name
]

{ #category : #accessing }
EpBehaviorCategoryChange >> classAffected [

	^ class
]

{ #category : #initialize }
EpBehaviorCategoryChange >> initializeOldCategory: anOldCategory newCategory: aNewCategory class: aClass [

	self initialize.
	oldCategory := anOldCategory.
	newCategory := aNewCategory.
	class := aClass asEpiceaRingDefinition.
]

{ #category : #accessing }
EpBehaviorCategoryChange >> newCategory [

	^ newCategory
]

{ #category : #accessing }
EpBehaviorCategoryChange >> oldCategory [

	^ oldCategory
]
