"
I'm a notification type information
"
Class {
	#name : #SpecNotificationTypeInfo,
	#superclass : #SpecNotificationType,
	#category : #'Spec-Core-Base'
}

{ #category : #dispatching }
SpecNotificationTypeInfo >> notify: aSpecNotification on: aNotificationAware [
	
	aNotificationAware notifyInfo: aSpecNotification
]
