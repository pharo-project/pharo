"
I represent a rename of a class or a trait.
"
Class {
	#name : #EpBehaviorNameChange,
	#superclass : #EpBehaviorChange,
	#instVars : [
		'oldName',
		'newName',
		'behavior'
	],
	#category : #Epicea-Model
}

{ #category : #'instance creation' }
EpBehaviorNameChange class >> oldName: anOldName newName: aNewName class: aClass [

	^ self basicNew 
		initializeOldName: anOldName newName: aNewName class: aClass;
		yourself
]

{ #category : #visitor }
EpBehaviorNameChange >> accept: aVisitor [

	^ aVisitor visitBehaviorNameChange: self 
]

{ #category : #accessing }
EpBehaviorNameChange >> behaviorAffected [
	^ self classAffected
]

{ #category : #accessing }
EpBehaviorNameChange >> behaviorAffectedName [
	^ self classAffected name
]

{ #category : #accessing }
EpBehaviorNameChange >> classAffected [

	^ behavior
]

{ #category : #initialization }
EpBehaviorNameChange >> initializeOldName: anOldName newName: aNewName class: aBehavior [

	self initialize.
	behavior := aBehavior asEpiceaRingDefinition.
	oldName := anOldName.
	newName := aNewName.
	
]

{ #category : #accessing }
EpBehaviorNameChange >> newName [

	^ newName
]

{ #category : #accessing }
EpBehaviorNameChange >> oldName [

	^ oldName
]
