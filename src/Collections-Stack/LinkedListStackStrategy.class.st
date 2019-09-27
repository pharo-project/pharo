"
I am a stack using a LinkedList under the hood.

The first link of the linked list is considered as the top of the stack as it is efficient to add / remove.
"
Class {
	#name : #LinkedListStackStrategy,
	#superclass : #AbstractStackStrategy,
	#instVars : [
		'linkedList'
	],
	#category : #'Collections-Stack-Strategies'
}

{ #category : #enumerating }
LinkedListStackStrategy >> do: aBlock [
 	linkedList do: aBlock
]

{ #category : #initialization }
LinkedListStackStrategy >> initialize [
 	super initialize.
 	linkedList := LinkedList new
]

{ #category : #removing }
LinkedListStackStrategy >> pop [
 	"We use the first link of the linked list as the top of the stack as it is efficient to add / remove."
 	^ linkedList removeFirst
]

{ #category : #removing }
LinkedListStackStrategy >> push: newObject [
 	linkedList addFirst: newObject.
 	^ newObject
]

{ #category : #accessing }
LinkedListStackStrategy >> size [
 	^ linkedList size
]

{ #category : #accessing }
LinkedListStackStrategy >> top [
 	^ linkedList first
]
