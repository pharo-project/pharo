"
I am a command to rename message selector.
In addition I allow to swap arguments in signature.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newSignature:		<SycMessageDescription>
"
Class {
	#name : #SycRenameMessageCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSignature'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #requesting }
SycRenameMessageCommand >> canAddArgs [ 
	^false
]

{ #category : #requesting }
SycRenameMessageCommand >> canRemoveArgs [ 
	^ false
]

{ #category : #execution }
SycRenameMessageCommand >> createRefactoring [

	^ (RBRenameMethodRefactoring
		model: model
		renameMethod: originalMessage selector 
		in: (SycMessageOriginHack of: originalMessage) "look at SycMessageOriginHack comment"
		to: newSignature selector 
		permutation: newSignature permutation)
		renameMap: newSignature renameMap
]

{ #category : #accessing }
SycRenameMessageCommand >> defaultMenuIconName [
	^#edit

]

{ #category : #accessing }
SycRenameMessageCommand >> defaultMenuItemName [
	^'Rename'
]

{ #category : #accessing }
SycRenameMessageCommand >> newSignature [
	^ newSignature
]

{ #category : #accessing }
SycRenameMessageCommand >> newSignature: anObject [
	newSignature := anObject
]

{ #category : #execution }
SycRenameMessageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.

	newSignature := self requestNewMessageIn: aToolContext
	

]

{ #category : #execution }
SycRenameMessageCommand >> resultMessageSelector [
	^newSignature selector
]
