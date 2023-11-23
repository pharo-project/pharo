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
		^ CoCaseSensitiveBeginsWithFilter new
			completionString: aString;
			yourself ]
		ifFalse: [
		^ CoCaseInsensitiveBeginsWithFilter new
			completionString: aString;
			yourself	]
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
