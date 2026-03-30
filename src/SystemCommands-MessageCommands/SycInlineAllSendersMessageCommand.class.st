"
I am a command to launch the Inline All Senders Refactoring: the call to this method in all other methods of this class is replaced by its implementation. 
"
Class {
	#name : 'SycInlineAllSendersMessageCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycInlineAllSendersMessageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : 'accessing' }
SycInlineAllSendersMessageCommand >> defaultMenuIconName [
	^#smallRightFlush
]

{ #category : 'accessing' }
SycInlineAllSendersMessageCommand >> defaultMenuItemName [
	^'Inline senders'
]

{ #category : 'execution' }
SycInlineAllSendersMessageCommand >> execute [

	| methodOrigin |
	methodOrigin := originalMessage contextUser compiledMethod origin.
	((ReInlineAllSendersDriver newApplication: self application) scopes: refactoringScopes message: originalMessage selector class: methodOrigin)
		runRefactoring 
]
