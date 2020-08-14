"
I represent a CompletionKeyboardEvent specialized for when the event's keyCharacter is $""
"
Class {
	#name : #CompletionDoubleQuoteKeyboardEvent,
	#superclass : #CompletionKeyboardEvent,
	#category : #'NECompletion-Morphic'
}

{ #category : #queries }
CompletionDoubleQuoteKeyboardEvent >> keyCharacterLeft [
	^ self preferencesClass smartCharactersLeftFor: self event keyCharacter 
]

{ #category : #queries }
CompletionDoubleQuoteKeyboardEvent >> keyCharacterRight [
	^ self preferencesClass smartCharactersRightFor: self event keyCharacter 
]

{ #category : #'actions - enclosing' }
CompletionDoubleQuoteKeyboardEvent >> replacementAfterEncloseActionOnSelection: selectedString [
	"Returns the replacement after either enclosing or unenclosing selectedString with double quotes"
	
	| l r |
	l := selectedString at: 1.
	r := selectedString at: selectedString size.
	
	((l = $") and: [ r = $" ])
		ifTrue: [  ^ self replacementAfterUnenclosingSelection: selectedString ].
		
	^ self replacementAfterEnclosingSelection: selectedString
]

{ #category : #'actions - enclosing' }
CompletionDoubleQuoteKeyboardEvent >> replacementAfterEnclosingSelection: selectedString [
	"Returns the replacement after enclosing selectedString with double quotes
	We do this by replacing every double quote in selectedString with two double quotes"
	
	"Another possible implementation would be to skip already-paired double quotes, 
	but being consistent allows us to un-enclose and restore the original selection.
	
	See class comment for examples (writing it here requires too many double quotes.)"
	
	| replaced |
	replaced := selectedString copyReplaceAll: '"' with:  '""'.
	^ '"' , replaced , '"'
]

{ #category : #'actions - enclosing' }
CompletionDoubleQuoteKeyboardEvent >> replacementAfterUnenclosingSelection: selectedString [
	"Returns the replacement after unenclosing selectedString with double quotes.
	
	This is essentially undoing replacementAfterEnclosingSelection:"
	
	| trimmed |
	"Even if selectedString does not have a double quote on either side, trimBoth will just trim the side that has the double quote"
	trimmed := selectedString trimBoth: [ :each | each = $" ].
	
	^ trimmed copyReplaceAll: '""' with:  '"'
]
