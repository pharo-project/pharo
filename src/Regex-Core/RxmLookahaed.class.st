"
Instance holds onto a lookead which matches but does not consume anything.

Instance variables:
	predicate		<RxmLink>
"
Class {
	#name : #RxmLookahaed,
	#superclass : #RxmLink,
	#instVars : [
		'lookahead',
		'positive'
	],
	#category : 'Regex-Core'
}

{ #category : #'instance creation' }
RxmLookahaed class >> with: aPiece [

	^self new lookahead: aPiece
]

{ #category : #initialization }
RxmLookahaed >> initialize [
	super initialize.
	positive := true.
]

{ #category : #accessing }
RxmLookahaed >> lookahead [
	^ lookahead
]

{ #category : #accessing }
RxmLookahaed >> lookahead: anRxmLink [
	lookahead := anRxmLink
]

{ #category : #matching }
RxmLookahaed >> matchAgainst: aMatcher [
	"Match if the predicate block evaluates to true when given the
	current stream character as the argument."

	| original result |
	original := aMatcher currentState.
	result := lookahead matchAgainst: aMatcher.
	aMatcher restoreState: original.
	^ result not 
		and: [ next matchAgainst: aMatcher ]
]

{ #category : #building }
RxmLookahaed >> terminateWith: aNode [
	lookahead terminateWith: aNode.
	super terminateWith: aNode.
]
