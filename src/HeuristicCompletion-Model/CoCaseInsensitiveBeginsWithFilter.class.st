"
The begins with can be case sensitive or not.
I am the implementation of insensitive comparison.

"
Class {
	#name : #CoCaseInsensitiveBeginsWithFilter,
	#superclass : #CoBeginsWithFilter,
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #'instance creation' }
CoCaseInsensitiveBeginsWithFilter class >> filterString: aString [

	^ self new
		completionString: aString;
		yourself
]

{ #category : #testing }
CoCaseInsensitiveBeginsWithFilter >> accepts: aCandidate [
	
	completionString ifEmpty: [ ^ true ].
	^ aCandidate contents asLowercase beginsWith: completionString asLowercase
]

{ #category : #testing }
CoCaseInsensitiveBeginsWithFilter >> isLessNarrowThanCaseInsensitive: anotherFilter [

	^ anotherFilter completionString beginsWith: self completionString
]

{ #category : #testing }
CoCaseInsensitiveBeginsWithFilter >> isLessNarrowThanCaseSensitive: anotherFilter [

	^ false
]

{ #category : #testing }
CoCaseInsensitiveBeginsWithFilter >> isMoreNarrowThan: anotherFilter [

	^ anotherFilter isLessNarrowThanCaseInsensitive: self
]
