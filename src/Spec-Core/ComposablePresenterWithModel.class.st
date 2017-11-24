"
I am a composable presenter that  keeps reference to its domain model (announcingObject) optionally wrapped in a value holder and subscribes yourself to it.

As the model (announcingObject) we can set a subclass of Model or a value holder (NewValueHolder). In that case such models are stored directly into announcingObject.

You should implement the method #modelChanged in my subclasses

"
Class {
	#name : #ComposablePresenterWithModel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'announcingObject'
	],
	#category : #'Spec-Core'
}

{ #category : #'accessing - private' }
ComposablePresenterWithModel >> announcingObject [

	^ announcingObject
]

{ #category : #accessing }
ComposablePresenterWithModel >> model [

	 ^ self announcingObject value
]

{ #category : #accessing }
ComposablePresenterWithModel >> model: aDomainObject [

	"aDomainObject can be regular object, a value holder (NewValueHolder) or an instance of Model"
	
	(aDomainObject isAnnouncingObject not and: [ self announcingObject isValueHolder ])
		ifTrue: [ self announcingObject value: aDomainObject ]
		ifFalse: [ self setAnnouncingObject: aDomainObject ]
	
]

{ #category : #initialization }
ComposablePresenterWithModel >> modelChanged [

	"subclass responsibility (optional)"
]

{ #category : #'accessing - private' }
ComposablePresenterWithModel >> setAnnouncingObject: aDomainObject [

	announcingObject := aDomainObject wrappedInValueHolderIfNeeded.
	announcingObject whenChangedDo: [ self modelChanged ].
	
]

{ #category : #initialization }
ComposablePresenterWithModel >> setModelBeforeInitialization: aDomainObject [

	self setAnnouncingObject: aDomainObject.

]

{ #category : #updating }
ComposablePresenterWithModel >> updatePresenter [

	self modelChanged 
]
