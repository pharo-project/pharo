"
The begins with can be case sensitive or not.
I am the implementation of sensitive comparison.

"
Class {
	#name : #CoCaseSensitiveBeginsWithFilter,
	#superclass : #CoBeginsWithFilter,
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #'instance creation' }
CoCaseSensitiveBeginsWithFilter class >> filterString: aString [

	^ self new
		completionString: aString;
		yourself
]

{ #category : #testing }
CoCaseSensitiveBeginsWithFilter >> accepts: aCandidate [
	
	completionString ifEmpty: [ ^ true ].
	^ aCandidate contents beginsWith: completionString
]

{ #category : #testing }
CoCaseSensitiveBeginsWithFilter >> isLessNarrowThanCaseInsensitive: anotherFilter [

	^ false
]

{ #category : #testing }
CoCaseSensitiveBeginsWithFilter >> isLessNarrowThanCaseSensitive: anotherFilter [

	^ anotherFilter completionString beginsWith: self completionString
]

{ #category : #testing }
CoCaseSensitiveBeginsWithFilter >> isMoreNarrowThan: anotherFilter [

	^ anotherFilter isLessNarrowThanCaseSensitive: self
]
