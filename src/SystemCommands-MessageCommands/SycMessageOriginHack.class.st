"
I represent class of message implementor for method refactorings.
Implementing class is required for current method refactorings but it should be removed in future because it should be always message refactoring. For example RBRenameMethodRefactoring renames any selectors in image which is not restrited by any existing method. Probably reason of such restriction was how browser implements rename of methods when user activated it on selected methods.
Same is valid for add/remove method parameter.
So now these  refactorings check that given class defines given selector which is not needed actually because we modify message.
I used in place of class for all such refactorings to allow them work on messages
"
Class {
	#name : #SycMessageOriginHack,
	#superclass : #Object,
	#instVars : [
		'message'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #'instance creation' }
SycMessageOriginHack class >> of: aMessageDescription [
	^self new 
		message: aMessageDescription 
]

{ #category : #'hacked methods' }
SycMessageOriginHack >> directlyDefinesMethod: aSymbol [
	^message selector == aSymbol
]

{ #category : #accessing }
SycMessageOriginHack >> message [
	^ message
]

{ #category : #accessing }
SycMessageOriginHack >> message: anObject [
	message := anObject
]

{ #category : #'hacked methods' }
SycMessageOriginHack >> name [
	^message contextUser printString
]

{ #category : #'hacked methods' }
SycMessageOriginHack >> parseTreeFor: aSelector [
	^message
]
