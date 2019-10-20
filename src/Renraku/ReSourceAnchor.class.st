"
A source anchor has a refference to the actual class, method, or other entity that is criticized. An anchor also has a #providesInterval method that returns a boolean indicating if the anchor provides a selection interval to the actual source of the critique. The interval can be accessed through the #interval method
"
Class {
	#name : #ReSourceAnchor,
	#superclass : #Object,
	#instVars : [
		'sourceEntity'
	],
	#category : #'Renraku-SourceAnchors'
}

{ #category : #'instance creation' }
ReSourceAnchor class >> entity: anEntity [ 
	^ self basicNew
		initializeEnitity: anEntity;
		yourself
]

{ #category : #accessing }
ReSourceAnchor >> entity [
	^ self sourceEntity
]

{ #category : #initialization }
ReSourceAnchor >> initializeEnitity: anEntity [ 
	
	sourceEntity := anEntity
]

{ #category : #accessing }
ReSourceAnchor >> interval [

	^ nil
]

{ #category : #testing }
ReSourceAnchor >> providesInterval [

	^ false
]

{ #category : #accessing }
ReSourceAnchor >> sourceEntity [
	^ sourceEntity
]
