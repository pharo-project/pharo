"
I am a command to launch the Inline All Senders Refactoring: the call to this method in all other methods of this class is replaced by its implementation. 
"
Class {
	#name : 'SycInlineIntoLocalSendersMessageCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'testing' }
SycInlineIntoLocalSendersMessageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : 'execution' }
SycInlineIntoLocalSendersMessageCommand >> execute [ 

	| methodOrigin |
	methodOrigin := originalMessage contextUser compiledMethod origin.
	((ReInlineIntoLocalSendersDriver newApplication: self application) scopes: refactoringScopes message: originalMessage selector class: methodOrigin)
		runRefactoring  
]

{ #category : 'accessing' }
SycInlineIntoLocalSendersMessageCommand >> iconName [
	^#smallRightFlush
]

{ #category : 'accessing' }
SycInlineIntoLocalSendersMessageCommand >> name [
	^'Inline senders'
]

{ #category : 'execution' }
SycInlineIntoLocalSendersMessageCommand >> resultMessageSelector [

	^ originalMessage contextUser compiledMethod selector
]
