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

{ #category : #execution }
SycRenameMessageCommand >> createRefactoring [
	^ RBRenameMethodRefactoring
		renameMethod: originalMessage selector 
		in: class
		to: newSignature selector 
		permutation: (originalMessage computeArgumentPermutationsFor: newSignature) 
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

	newSignature := self requestNewMethodName.
	

]

{ #category : #execution }
SycRenameMessageCommand >> requestNewMethodName [
	| copyMethodName dialog |
	copyMethodName := originalMessage copy.
	dialog := SycMethodNameEditorPresenter openOn: copyMethodName.
	dialog cancelled ifTrue: [  CmdCommandAborted signal ].
	copyMethodName selector = originalMessage selector & 
		(copyMethodName arguments = originalMessage arguments)
			ifTrue: [ CmdCommandAborted signal].		
				
	^ RBMethodName selector: copyMethodName selector arguments: copyMethodName arguments

]

{ #category : #execution }
SycRenameMessageCommand >> resultMessageSelector [
	^newSignature selector
]
