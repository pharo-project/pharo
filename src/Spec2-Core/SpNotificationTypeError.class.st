"
I'm a notification type error
"
Class {
	#name : #SpNotificationTypeError,
	#superclass : #SpNotificationType,
	#category : #'Spec2-Core-Base'
}

{ #category : #dispatching }
SpNotificationTypeError >> notify: aSpecNotification on: aNotificationAware [
	
	aNotificationAware notifyError: aSpecNotification
]
