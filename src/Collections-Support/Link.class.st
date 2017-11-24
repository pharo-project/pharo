"
An instance of me is a simple record of a pointer to another Link. I am an abstract class; my concrete subclasses, for example, Process, can be stored in a LinkedList structure.
"
Class {
	#name : #Link,
	#superclass : #Object,
	#instVars : [
		'nextLink'
	],
	#category : #'Collections-Support'
}

{ #category : #'instance creation' }
Link class >> nextLink: aLink [ 
	"Answer an instance of me referring to the argument, aLink."

	^self new nextLink: aLink; yourself
]

{ #category : #converting }
Link >> asLink [
	^self
]

{ #category : #accessing }
Link >> nextLink [
	"Answer the link to which the receiver points."

	^nextLink
]

{ #category : #accessing }
Link >> nextLink: aLink [ 
	"Store the argument, aLink, as the link to which the receiver refers. 
	Answer aLink."

	^nextLink := aLink
]
