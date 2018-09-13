"
I am here for historical reasons, now deprecated.

You should use LinkedListStack if you want a stack using a linked list under the hood.
"
Class {
	#name : #Stack,
	#superclass : #LinkedListStack,
	#category : #Deprecated70
}

{ #category : #deprecation }
Stack class >> isDeprecated [
	^ true
]
