"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
One of a few special nodes more often representing special state of the match rather than a predicate on a character.  The ugly exception is the #any condition which *is* a predicate on a character.

Instance variables:
	kind		<Selector>
"
Class {
	#name : #RxsContextCondition,
	#superclass : #RxsNode,
	#instVars : [
		'kind'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #'initialize-release' }
RxsContextCondition >> beAny [
	"Matches anything but a newline."

	kind := #syntaxAny
]

{ #category : #initialization }
RxsContextCondition >> beBeginningOfLine [
	"Matches empty string at the beginning of a line."

	kind := #syntaxBeginningOfLine
]

{ #category : #initialization }
RxsContextCondition >> beBeginningOfWord [
	"Matches empty string at the beginning of a word."

	kind := #syntaxBeginningOfWord
]

{ #category : #initialization }
RxsContextCondition >> beEndOfLine [
	"Matches empty string at the end of a line."

	kind := #syntaxEndOfLine
]

{ #category : #initialization }
RxsContextCondition >> beEndOfWord [
	"Matches empty string at the end of a word."

	kind := #syntaxEndOfWord
]

{ #category : #initialization }
RxsContextCondition >> beNonWordBoundary [
	"Analog of \B."

	kind := #syntaxNonWordBoundary
]

{ #category : #initialization }
RxsContextCondition >> beWordBoundary [
	"Analog of \w (alphanumeric plus _)."

	kind := #syntaxWordBoundary
]

{ #category : #accessing }
RxsContextCondition >> dispatchTo: aBuilder [

	^aBuilder perform: kind
]

{ #category : #testing }
RxsContextCondition >> isNullable [

	^#syntaxAny ~~ kind
]
