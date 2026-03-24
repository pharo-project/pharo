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

{ #category : 'matching' }
CoBeginsWithFilter >> candidate: aCandidate matchesCamelCaseCaseSensitive: aBoolean [

	^ (aCandidate respondsTo: #matchesCamelCaseCompletionToken:caseSensitive:)
		and: [ aCandidate
			matchesCamelCaseCompletionToken: completionString
			caseSensitive: aBoolean ]
]

{ #category : 'matching' }
CoBeginsWithFilter >> candidate: aCandidate matchesPrefixCaseSensitive: aBoolean [

	^ (aCandidate respondsTo: #matchesPrefixToken:caseSensitive:)
		ifTrue: [ aCandidate matchesPrefixToken: completionString caseSensitive: aBoolean ]
		ifFalse: [ aCandidate asString beginsWith: completionString caseSensitive: aBoolean ]
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
