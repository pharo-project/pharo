"
This class is private to the EHS implementation.  An instance of it is signaled whenever an attempt is made to resume from an exception which answers false to #isResumable.
"
Class {
	#name : #IllegalResumeAttempt,
	#superclass : #Exception,
	#category : #'Kernel-Exceptions'
}

{ #category : #comment }
IllegalResumeAttempt >> isResumable [
	
	^ false
]

{ #category : #comment }
IllegalResumeAttempt >> readMe [

	"Never handle this exception!"
]
