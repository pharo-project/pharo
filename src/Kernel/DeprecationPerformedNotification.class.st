"
I am a notificaton about an operation that was performed as reaction of a Deprecation warning. I inform user about a method rewrite and so on. Usual reaction is to write related description to Transcript.
"
Class {
	#name : #DeprecationPerformedNotification,
	#superclass : #Notification,
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptiondescription }
DeprecationPerformedNotification >> defaultAction [
	
	UIManager default deprecationPerformedNotificationDefaultAction: self
]
