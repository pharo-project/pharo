"
I am raised when a window has been built
"
Class {
	#name : #WindowBuilt,
	#superclass : #Announcement,
	#instVars : [
		'model'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #'instance creation' }
WindowBuilt class >> model: aWindowPresenter [

	^ self new 
		model: aWindowPresenter;
		yourself
]

{ #category : #accessing }
WindowBuilt >> model [ 

	^ model
]

{ #category : #accessing }
WindowBuilt >> model: aModel [

	model := aModel
]
