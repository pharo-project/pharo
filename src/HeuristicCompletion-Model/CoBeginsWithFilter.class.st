"
I am representing the filters with a begins with.

"
Class {
	#name : 'CoBeginsWithFilter',
	#superclass : 'CoFilter',
	#instVars : [
		'completionString'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'instance creation' }
CoBeginsWithFilter class >> caseSensitive: aBoolean filterString: aString [

	aBoolean ifTrue: [
		^ CoToleranceFilter new 
			decoree: (CoCaseSensitiveBeginsWithFilter new
				completionString: aString; yourself) 
				; yourself ]
		ifFalse: [
		^ CoToleranceFilter new 
			decoree: (CoCaseInsensitiveBeginsWithFilter new
			completionString: aString;
			yourself); yourself 	]
]

{ #category : 'testing' }
CoBeginsWithFilter >> complementResults: aCollection [
	"give a chance to the filter to complement results after the fact.
	Called by fetch:. By default do nothing."
	
	^ aCollection
]

{ #category : 'accessing' }
CoBeginsWithFilter >> completionString [

	^ completionString
]

{ #category : 'accessing' }
CoBeginsWithFilter >> completionString: anObject [

	completionString := anObject
]

{ #category : 'testing' }
CoBeginsWithFilter >> isLessNarrowThanNegation: anotherFilter [

	^ false
]
