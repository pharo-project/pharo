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
	#category : 'Regex-Core'
}

{ #category : #initialize-release }
RxmSpecial >> beBeginningOfLine [

	matchSelector := #atBeginningOfLine
]

{ #category : #initialize-release }
RxmSpecial >> beBeginningOfWord [

	matchSelector := #atBeginningOfWord
]

{ #category : #initialize-release }
RxmSpecial >> beEndOfLine [

	matchSelector := #atEndOfLine
]

{ #category : #initialize-release }
RxmSpecial >> beEndOfWord [

	matchSelector := #atEndOfWord
]

{ #category : #initialize-release }
RxmSpecial >> beNotWordBoundary [

	matchSelector := #notAtWordBoundary
]

{ #category : #initialize-release }
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
