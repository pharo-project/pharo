"
I generate a name by attaching a suffix after last dot in the name received as argument. 

My subclasses implement the concrete ways to do it.
"
Class {
	#name : #OmSuffixAfterDotStrategy,
	#superclass : #OmSessionStoreNamingStrategy,
	#category : 'Ombu-Strategies'
}

{ #category : #accessing }
OmSuffixAfterDotStrategy >> nextSuffix [

	^ self subclassResponsibility
]

{ #category : #accessing }
OmSuffixAfterDotStrategy >> nextTo: aName in: aDirectory [
	
	^ String streamContents: [ :stream |
		stream
			nextPutAll: (aName copyUpToLast: $.);
			nextPut: $.;
			nextPutAll: self nextSuffix ]
]
