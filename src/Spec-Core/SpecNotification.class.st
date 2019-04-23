"
I'm a notification.
I can be dispatched to an application and the application will deliver it as it corresponds.

"
Class {
	#name : #SpecNotification,
	#superclass : #Object,
	#instVars : [
		'type',
		'message'
	],
	#category : #'Spec-Core-Base'
}

{ #category : #'instance creation' }
SpecNotification class >> dispatchErrorTo: aNotificationCenter message: aString [ 

	^ self new 
		beError;
		message: aString;
		dispatchTo: aNotificationCenter
]

{ #category : #'instance creation' }
SpecNotification class >> dispatchInfoTo: aNotificationCenter message: aString [ 

	^ self new 
		beInfo;
		message: aString;
		dispatchTo: aNotificationCenter
]

{ #category : #accessing }
SpecNotification >> beError [

	self type: SpecNotificationType error
]

{ #category : #accessing }
SpecNotification >> beInfo [ 

	self type: SpecNotificationType info
]

{ #category : #dispatching }
SpecNotification >> dispatchTo: aNotificationCenter [

	^ aNotificationCenter notify: self
]

{ #category : #initialization }
SpecNotification >> initialize [

	super initialize.
	self type: SpecNotificationType info.
]

{ #category : #accessing }
SpecNotification >> message [
	^ message
]

{ #category : #accessing }
SpecNotification >> message: anObject [
	message := anObject
]

{ #category : #accessing }
SpecNotification >> type [
	^ type
]

{ #category : #accessing }
SpecNotification >> type: anObject [
	type := anObject
]
