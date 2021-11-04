Class {
	#name : #CoBeginsWithFilter,
	#superclass : #CoFilter,
	#instVars : [
		'completionString'
	],
	#category : #'HeuristicCompletion-Model-Core'
}

{ #category : #'instance creation' }
CoBeginsWithFilter class >> caseSensitive: aBoolean filterString: aString [ 

	aBoolean ifTrue: [ 
		^ CoCaseSensitiveBeginsWithFilter new
			completionString: aString;
			yourself ].
	self halt
]

{ #category : #accessing }
CoBeginsWithFilter >> completionString [

	^ completionString
]

{ #category : #accessing }
CoBeginsWithFilter >> completionString: anObject [

	completionString := anObject
]
