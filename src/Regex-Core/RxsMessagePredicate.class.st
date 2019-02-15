"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A message predicate represents a condition on a character that is tested (at the match time) by sending a unary message to the character expecting a Boolean answer.

Instance variables:
	selector		<Symbol>
"
Class {
	#name : #RxsMessagePredicate,
	#superclass : #RxsNode,
	#instVars : [
		'selector',
		'negated'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #accessing }
RxsMessagePredicate >> dispatchTo: aBuilder [
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."

	^aBuilder syntaxMessagePredicate: self
]

{ #category : #initialization }
RxsMessagePredicate >> initializeSelector: aSelector [
	"The selector must be a one-argument message understood by Character."

	selector := aSelector
]

{ #category : #initialization }
RxsMessagePredicate >> initializeSelector: aSelector negated: aBoolean [
	"The selector must be a one-argument message understood by Character."

	selector := aSelector.
	negated := aBoolean
]

{ #category : #accessing }
RxsMessagePredicate >> negated [

	^negated
]

{ #category : #accessing }
RxsMessagePredicate >> selector [

	^selector
]
