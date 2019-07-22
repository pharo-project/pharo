"
I am raised when a window has been built
"
Class {
	#name : #SpWindowBuilt,
	#superclass : #Announcement,
	#instVars : [
		'model'
	],
	#category : #'Spec2-Core-Support'
}

{ #category : #'instance creation' }
SpWindowBuilt class >> model: aWindowPresenter [

	^ self new 
		model: aWindowPresenter;
		yourself
]

{ #category : #accessing }
SpWindowBuilt >> model [ 

	^ model
]

{ #category : #accessing }
SpWindowBuilt >> model: aModel [

	model := aModel
]
