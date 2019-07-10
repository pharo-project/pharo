"
I'm a notification type error
"
Class {
	#name : #SpecNotificationTypeError,
	#superclass : #SpecNotificationType,
	#category : #'Spec-Core-Base'
}

{ #category : #dispatching }
SpecNotificationTypeError >> notify: aSpecNotification on: aNotificationAware [
	
	aNotificationAware notifyError: aSpecNotification
]
