"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A character is a literal character that appears either in the expression itself or in a character set within an expression.

Instance variables:
	character		<Character>
"
Class {
	#name : #RxsCharacter,
	#superclass : #RxsNode,
	#instVars : [
		'character'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #'instance creation' }
RxsCharacter class >> with: aCharacter [

	^self new initializeCharacter: aCharacter
]

{ #category : #accessing }
RxsCharacter >> character [

	^character
]

{ #category : #accessing }
RxsCharacter >> dispatchTo: aMatcher [
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."

	^aMatcher syntaxCharacter: self
]

{ #category : #accessing }
RxsCharacter >> enumerateTo: aSet ignoringCase: aBoolean [

	aBoolean
		ifTrue: 
			[aSet 
				add: character asUppercase;
				add: character asLowercase]
		ifFalse: [aSet add: character]
]

{ #category : #initialization }
RxsCharacter >> initializeCharacter: aCharacter [
	"See class comment for instance variable description."

	character := aCharacter
]

{ #category : #testing }
RxsCharacter >> isAtomic [
	"A character is always atomic."

	^true
]

{ #category : #testing }
RxsCharacter >> isEnumerable [

	^true
]
