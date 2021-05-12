"
I'm a notification type error
"
Class {
	#name : #SpErrorNotificationType,
	#superclass : #SpNotificationType,
	#category : #'Spec2-Core-Base'
}

{ #category : #dispatching }
SpErrorNotificationType >> notify: aSpecNotification on: aNotificationAware [
	
	aNotificationAware notifyError: aSpecNotification
]
