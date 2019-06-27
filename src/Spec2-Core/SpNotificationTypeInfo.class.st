"
I'm a notification type information
"
Class {
	#name : #SpNotificationTypeInfo,
	#superclass : #SpNotificationType,
	#category : #'Spec2-Core-Base'
}

{ #category : #dispatching }
SpNotificationTypeInfo >> notify: aSpecNotification on: aNotificationAware [
	
	aNotificationAware notifyInfo: aSpecNotification
]
