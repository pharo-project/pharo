"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
This is a branch of a matching process. Either `next' chain should match, or `alternative', if not nil, should match. Since this is also used to build loopbacks to match repetitions, `loopback' variable indicates whether the instance is a loopback: it affects the matcher-building operations (which of the paths through the branch is to consider as the primary when we have to find the ""tail"" of a matcher construct).

Instance variables
	alternative		<RxmLink> to match if `next' fails to match.
	loopback		<Boolean>
"
Class {
	#name : #RxmBranch,
	#superclass : #RxmLink,
	#instVars : [
		'loopback',
		'alternative'
	],
	#category : #'Regex-Core-Links'
}

{ #category : #initialization }
RxmBranch >> alternative: aBranch [
	"See class comment for instance variable description."

	alternative := aBranch
]

{ #category : #initialization }
RxmBranch >> beLoopback [
	"See class comment for instance variable description."

	loopback := true
]

{ #category : #initialization }
RxmBranch >> initialize [
	"See class comment for instance variable description."

	super initialize.
	loopback := false
]

{ #category : #matching }
RxmBranch >> matchAgainst: aMatcher [
	"Match either `next' or `alternative'. Fail if the alternative is nil."

	^(next matchAgainst: aMatcher)
		or: [alternative notNil
			and: [alternative matchAgainst: aMatcher]]
]

{ #category : #building }
RxmBranch >> pointTailTo: aNode [
	"See superclass for explanations."

	loopback
		ifTrue: [
			alternative == nil
				ifTrue: [alternative := aNode]
				ifFalse: [alternative pointTailTo: aNode]]
		ifFalse: [super pointTailTo: aNode]
]

{ #category : #building }
RxmBranch >> terminateWith: aNode [
	"See superclass for explanations."

	loopback
		ifTrue: [alternative == nil
			ifTrue: [alternative := aNode]
			ifFalse: [alternative terminateWith: aNode]]
		ifFalse: [super terminateWith: aNode]
]
