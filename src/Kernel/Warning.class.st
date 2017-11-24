"
A Warning is a Notification which by default should be brought to the attention of the user.
"
Class {
	#name : #Warning,
	#superclass : #Notification,
	#category : #'Kernel-Exceptions'
}

{ #category : #exceptionDescription }
Warning >> defaultAction [
	
	UIManager default warningDefaultAction: self
]
