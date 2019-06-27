"
I am a composable presenter that  keeps reference to its domain model (announcingObject) optionally wrapped in a value holder and subscribes yourself to it.

As the model (announcingObject) we can set a subclass of Model or a value holder (NewValueHolder). In that case such models are stored directly into announcingObject.

You should implement the method #modelChanged in my subclasses

"
Class {
	#name : #SpPresenterWithModel,
	#superclass : #SpPresenter,
	#instVars : [
		'announcingObject'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #'accessing - private' }
SpPresenterWithModel >> announcingObject [

	^ announcingObject
]

{ #category : #accessing }
SpPresenterWithModel >> model [

	 ^ self announcingObject value
]

{ #category : #accessing }
SpPresenterWithModel >> model: aDomainObject [

	"aDomainObject can be regular object, a value holder (NewValueHolder) or an instance of Model"
	
	(aDomainObject isSpAnnouncingObject not and: [ self announcingObject isValueHolder ])
		ifTrue: [ self announcingObject value: aDomainObject ]
		ifFalse: [ self setAnnouncingObject: aDomainObject ]
	
]

{ #category : #initialization }
SpPresenterWithModel >> modelChanged [

	"subclass responsibility (optional)"
]

{ #category : #'accessing - private' }
SpPresenterWithModel >> setAnnouncingObject: aDomainObject [

	announcingObject := aDomainObject isSpAnnouncingObject ifFalse: [ aDomainObject asValueHolder ] ifTrue: [ aDomainObject ].
	announcingObject whenChangedDo: [ self modelChanged ].
	
]

{ #category : #initialization }
SpPresenterWithModel >> setModelBeforeInitialization: aDomainObject [

	self setAnnouncingObject: aDomainObject.

]

{ #category : #updating }
SpPresenterWithModel >> updatePresenter [

	self modelChanged 
]
