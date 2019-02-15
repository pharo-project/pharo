"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A marker is used to remember positions of match of certain points of a regular expression. The marker receives an identifying key from the Matcher and uses that key to report positions of successful matches to the Matcher.

Instance variables:
	index	<Object> Something that makes sense for the Matcher. Received from the latter during initalization and later passed to it to identify the receiver.
"
Class {
	#name : #RxmMarker,
	#superclass : #RxmLink,
	#instVars : [
		'index'
	],
	#category : #'Regex-Core-Links'
}

{ #category : #initialization }
RxmMarker >> index: anIndex [
	"An index is a key that makes sense for the matcher.
	This key can be passed to marker position getters and
	setters to access position for this marker in the current
	matching session."

	index := anIndex
]

{ #category : #matching }
RxmMarker >> matchAgainst: aMatcher [
	"If the rest of the link chain matches successfully, report the
	position of the stream *before* the match started to the matcher."

	| startPosition |
	startPosition := aMatcher position.
	(next matchAgainst: aMatcher)
		ifTrue:
			[aMatcher markerPositionAt: index add: startPosition.
			^true].
	^false
]
