"
I implement a decorator that is useful with the string comparison.
I implement a selection based in the first letter of the filteringText.
I have two iterators inside, one for lowercase and other for uppercase text.

If the text starts with a lowercase letter, I will iterate the values first from the lowercase, and the from the uppercase.
If the text starts with an uppercase letter, I will iterate the values first from the uppercase, and the from the lowercase.

"
Class {
	#name : #SptCaseSelectorIterator,
	#superclass : #SptIteratorDecorator,
	#instVars : [
		'lowercaseIterator',
		'uppercaseIterator'
	],
	#category : #'GT-Spotter-Processors-Iterators'
}

{ #category : #protected }
SptCaseSelectorIterator >> doReset [

	lowercaseIterator reset.
	uppercaseIterator reset.
]

{ #category : #filtering }
SptCaseSelectorIterator >> filteringText: aString [
	aString
		ifEmpty: [ inner := lowercaseIterator.
			^ self ].

	inner := aString first isLowercase
		ifTrue: [ lowercaseIterator , uppercaseIterator ]
		ifFalse: [ uppercaseIterator , lowercaseIterator ].
]

{ #category : #accessing }
SptCaseSelectorIterator >> lowercaseIterator [
	^ lowercaseIterator
]

{ #category : #accessing }
SptCaseSelectorIterator >> lowercaseIterator: anObject [
	lowercaseIterator := anObject
]

{ #category : #accessing }
SptCaseSelectorIterator >> uppercaseIterator [
	^ uppercaseIterator
]

{ #category : #accessing }
SptCaseSelectorIterator >> uppercaseIterator: anObject [
	uppercaseIterator := anObject
]
