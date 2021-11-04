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
