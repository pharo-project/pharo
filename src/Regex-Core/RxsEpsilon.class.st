"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
This is an empty string.  It terminates some of the recursive constructs.
"
Class {
	#name : #RxsEpsilon,
	#superclass : #RxsNode,
	#category : 'Regex-Core'
}

{ #category : #building }
RxsEpsilon >> dispatchTo: aBuilder [
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."

	^aBuilder syntaxEpsilon
]

{ #category : #testing }
RxsEpsilon >> isNullable [
	"See comment in the superclass."

	^true
]
