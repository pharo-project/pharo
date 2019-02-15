"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
Instance holds onto a one-argument block and matches exactly one character if the block evaluates to true when passed the character as the argument.

Instance variables:
	predicate		<BlockClosure>
"
Class {
	#name : #RxmPredicate,
	#superclass : #RxmLink,
	#instVars : [
		'predicate'
	],
	#category : #'Regex-Core-Links'
}

{ #category : #'instance creation' }
RxmPredicate class >> with: unaryBlock [

	^self new predicate: unaryBlock
]

{ #category : #initialization }
RxmPredicate >> bePerform: aSelector [
	"Match any single character that answers true  to this message."

	self predicate: 
		[:char | 
		RxParser doHandlingMessageNotUnderstood: [char perform: aSelector]]
]

{ #category : #initialization }
RxmPredicate >> bePerformNot: aSelector [
	"Match any single character that answers false to this message."

	self predicate: 
		[:char | 
		RxParser doHandlingMessageNotUnderstood: [(char perform: aSelector) not]]
]

{ #category : #matching }
RxmPredicate >> matchAgainst: aMatcher [
	"Match if the predicate block evaluates to true when given the
	current stream character as the argument."

	| original |
	original := aMatcher currentState.
	(aMatcher atEnd not 
		and: [(predicate value: aMatcher next)
			and: [next matchAgainst: aMatcher]])
		ifTrue: [^true]
		ifFalse:
			[aMatcher restoreState: original.
			^false]
]

{ #category : #initialization }
RxmPredicate >> predicate: aBlock [
	"This link will match any single character for which <aBlock>
	evaluates to true."

	aBlock numArgs ~= 1 ifTrue: [self error: 'bad predicate block'].
	predicate := aBlock.
	^self
]
