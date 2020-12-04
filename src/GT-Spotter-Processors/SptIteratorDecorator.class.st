"
I implement an abstract decorator for the implementors.
I have an iterator in the inner instance variable and all the messages are redirected to it. 
My subclasses can override the required behaviour.
"
Class {
	#name : #SptIteratorDecorator,
	#superclass : #SptIterator,
	#instVars : [
		'inner'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #testing }
SptIteratorDecorator >> atEnd [ 

	^ inner atEnd
]

{ #category : #protected }
SptIteratorDecorator >> doReset [ 
	
	inner reset
]

{ #category : #accessing }
SptIteratorDecorator >> inner: anObject [
	inner := anObject
]

{ #category : #accessing }
SptIteratorDecorator >> next [

	^ inner next
]
