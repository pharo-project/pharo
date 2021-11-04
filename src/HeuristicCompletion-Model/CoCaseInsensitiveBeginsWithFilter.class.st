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
