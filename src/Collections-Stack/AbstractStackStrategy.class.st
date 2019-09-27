"
I am an abstract stack strategy. My subclasses can be used as Stack internal implementation.

I define the common API to be understood by stack strategies:
 - #push: to push an object at the top of the stack.
 - #top to get the object at the top of the stack without removing it.
 - #pop to remove the object at the top of the stack and return it.
 - #do: to iterate on stack elements from top to bottom.
 - #size to get the size of the stack.
"
Class {
	#name : #AbstractStackStrategy,
	#superclass : #Object,
	#category : #'Collections-Stack-Strategies'
}

{ #category : #enumerating }
AbstractStackStrategy >> do: aBlock [
 	"Evaluate aBlock with each of the receiver's elements as the argument.
 	 Starts by the element at the top of the Linked list and goes to the deepest element."

  	self subclassResponsibility
]

{ #category : #removing }
AbstractStackStrategy >> pop [
 	"Remove the object at the top of the stack and returns it.
 	 The implementation depends on the internal representation of the stack.
 	 Any subclass should implement this method properly."
 	^ self subclassResponsibility
]

{ #category : #adding }
AbstractStackStrategy >> push: newObject [
 	"Push newObject on the top of the stack.
 	 The implementation depends on the internal representation of the stack.
 	 Any subclass should implement this method properly."
 	self subclassResponsibility
]

{ #category : #accessing }
AbstractStackStrategy >> size [
	"Returns the size of the stack."
 	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractStackStrategy >> top [
 	"Returns the element at the top of the stack without removing it.
 	 The implementation depends on the internal representation of the stack.
 	 Any subclass should implement this method properly."
 	^ self subclassResponsibility
]
