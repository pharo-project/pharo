"
A Notification is an indication that something interesting has occurred.  If it is not handled, it will pass by without effect.
"
Class {
	#name : #Notification,
	#superclass : #Exception,
	#category : 'Kernel-Exceptions'
}

{ #category : #exceptiondescription }
Notification >> defaultAction [
	"No action is taken. The value nil is returned as the value of the message that signaled the exception."

	^nil
]
