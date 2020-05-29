"
I am sequence iterator, I will take all the iterators I have in myself and then I will go over the elements of them.
I iterate first all elements from the first one, and then the elements of the second one, and so on.
"
Class {
	#name : #SptSequenceIterator,
	#superclass : #SptIterator,
	#instVars : [
		'iterators'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #'instance creation' }
SptSequenceIterator class >> with: aCollection [ 
	
	^ self new
		iterators: aCollection;
		yourself
]

{ #category : #testing }
SptSequenceIterator >> atEnd [ 

	^ iterators allSatisfy: [ :e | e atEnd ]
]

{ #category : #protected }
SptSequenceIterator >> doReset [

	^ iterators do: [ :e | e reset ]
]

{ #category : #accessing }
SptSequenceIterator >> iterators: aCollection [ 

	iterators := aCollection 
]

{ #category : #accessing }
SptSequenceIterator >> next [

	iterators do: [ :e | e next ifNotNil: [ :aValue | ^ aValue ] ].
	^ nil
]
