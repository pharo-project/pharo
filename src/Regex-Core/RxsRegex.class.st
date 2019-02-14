"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
The body of a parenthesized thing, or a top-level expression, also an atom.  

Instance variables:
	branch		<RxsBranch>
	regex		<RxsRegex | RxsEpsilon>
"
Class {
	#name : #RxsRegex,
	#superclass : #RxsNode,
	#instVars : [
		'branch',
		'regex'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #accessing }
RxsRegex >> branch [

	^branch
]

{ #category : #accessing }
RxsRegex >> dispatchTo: aMatcher [
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."

	^aMatcher syntaxRegex: self
]

{ #category : #initialization }
RxsRegex >> initializeBranch: aBranch regex: aRegex [
	"See class comment for instance variable description."

	branch := aBranch.
	regex := aRegex
]

{ #category : #testing }
RxsRegex >> isNullable [

	^branch isNullable or: [regex notNil and: [regex isNullable]]
]

{ #category : #accessing }
RxsRegex >> regex [
	^regex
]
