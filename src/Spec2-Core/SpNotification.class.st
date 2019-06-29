"
I'm a notification.
I can be dispatched to an application and the application will deliver it as it corresponds.

"
Class {
	#name : #SpNotification,
	#superclass : #Object,
	#instVars : [
		'type',
		'message'
	],
	#category : #'Spec2-Core-Base'
}

{ #category : #'instance creation' }
SpNotification class >> dispatchErrorTo: aNotificationCenter message: aString [ 

	^ self new 
		beError;
		message: aString;
		dispatchTo: aNotificationCenter
]

{ #category : #'instance creation' }
SpNotification class >> dispatchInfoTo: aNotificationCenter message: aString [ 

	^ self new 
		beInfo;
		message: aString;
		dispatchTo: aNotificationCenter
]

{ #category : #accessing }
SpNotification >> beError [

	self type: SpNotificationType error
]

{ #category : #accessing }
SpNotification >> beInfo [ 

	self type: SpNotificationType info
]

{ #category : #dispatching }
SpNotification >> dispatchTo: aNotificationCenter [

	^ aNotificationCenter notify: self
]

{ #category : #initialization }
SpNotification >> initialize [

	super initialize.
	self type: SpNotificationType info.
]

{ #category : #accessing }
SpNotification >> message [
	^ message
]

{ #category : #accessing }
SpNotification >> message: anObject [
	message := anObject
]

{ #category : #accessing }
SpNotification >> type [
	^ type
]

{ #category : #accessing }
SpNotification >> type: anObject [
	type := anObject
]
