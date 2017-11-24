"
I am signalled by #duration:timeoutDo: if the receiving block takes too long to execute.

I am signalled by a watchdog process spawned by #duration:timeoutDo: and caught in the same method. 

I am not intended to be used elsewhere.
"
Class {
	#name : #TimedOut,
	#superclass : #Notification,
	#category : #'Kernel-Exceptions'
}
