"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A special node that matches a specific matcher state rather than any input character.
The state is either at-beginning-of-line or at-end-of-line.
"
Class {
	#name : #RxmSpecial,
	#superclass : #RxmLink,
	#instVars : [
		'matchSelector'
	],
	#category : #'Regex-Core-Links'
}

{ #category : #'initialize-release' }
RxmSpecial >> beBeginningOfLine [

	matchSelector := #atBeginningOfLine
]

{ #category : #initialization }
RxmSpecial >> beBeginningOfWord [

	matchSelector := #atBeginningOfWord
]

{ #category : #initialization }
RxmSpecial >> beEndOfLine [

	matchSelector := #atEndOfLine
]

{ #category : #initialization }
RxmSpecial >> beEndOfWord [

	matchSelector := #atEndOfWord
]

{ #category : #initialization }
RxmSpecial >> beNotWordBoundary [

	matchSelector := #notAtWordBoundary
]

{ #category : #initialization }
RxmSpecial >> beWordBoundary [

	matchSelector := #atWordBoundary
]

{ #category : #matching }
RxmSpecial >> matchAgainst: aMatcher [
	"Match without consuming any input, if the matcher is
	in appropriate state."

	^(aMatcher perform: matchSelector)
		and: [next matchAgainst: aMatcher]
]
