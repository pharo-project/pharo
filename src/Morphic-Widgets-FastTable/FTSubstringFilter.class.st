"
I filter given strings by checking substrings inclusion
"
Class {
	#name : #FTSubstringFilter,
	#superclass : #FTFilter,
	#category : #Morphic-Widgets-FastTable-Functions
}

{ #category : #matching }
FTSubstringFilter >> matches: aString [
	^aString includesSubstring: pattern caseSensitive: false
]
