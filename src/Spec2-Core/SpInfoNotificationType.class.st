"
I'm a notification type information
"
Class {
	#name : #SpInfoNotificationType,
	#superclass : #SpNotificationType,
	#category : #'Spec2-Core-Base'
}

{ #category : #dispatching }
SpInfoNotificationType >> notify: aSpecNotification on: aNotificationAware [
	
	aNotificationAware notifyInfo: aSpecNotification
]
