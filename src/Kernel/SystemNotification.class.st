"
I am a notification that informs user aboout some important operation that the system did. 
"
Class {
	#name : #SystemNotification,
	#superclass : #Notification,
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptiondescription }
SystemNotification >> defaultAction [
	
	UIManager default systemNotificationDefaultAction: self
]
