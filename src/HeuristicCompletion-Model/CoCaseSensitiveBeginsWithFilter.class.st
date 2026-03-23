"
The begins with can be case sensitive or not.
I am the implementation of sensitive comparison.

"
Class {
	#name : 'CoCaseSensitiveBeginsWithFilter',
	#superclass : 'CoBeginsWithFilter',
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'instance creation' }
CoCaseSensitiveBeginsWithFilter class >> filterString: aString [

	^ self new
		completionString: aString;
		yourself
]

{ #category : 'testing' }
CoCaseSensitiveBeginsWithFilter >> accepts: aCandidate [

	^ (self prefixAcceptsContents: aCandidate contents)
		or: [ CoCompletionTolerance enabled
			and: [ self fuzzyAcceptsContents: aCandidate contents ] ]
]

{ #category : 'testing' }
CoCaseSensitiveBeginsWithFilter >> isCaseSensitive [

	^ false
]

{ #category : 'testing' }
CoCaseSensitiveBeginsWithFilter >> isLessNarrowThanCaseInsensitive: anotherFilter [

	^ false
]

{ #category : 'testing' }
CoCaseSensitiveBeginsWithFilter >> isLessNarrowThanCaseSensitive: anotherFilter [

	^ anotherFilter completionString beginsWith: self completionString
]

{ #category : 'testing' }
CoCaseSensitiveBeginsWithFilter >> isMoreNarrowThan: anotherFilter [

	^ anotherFilter isLessNarrowThanCaseSensitive: self
]

{ #category : 'testing' }
CoCaseSensitiveBeginsWithFilter >> prefixAcceptsContents: aString [

	^ aString asLowercase beginsWith: completionString asLowercase
]
