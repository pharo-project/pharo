"
I am a command to launch the Inline All Senders Refactoring: the call to this method in all other methods of this class is replaced by its implementation. 
"
Class {
	#name : #SycInlineAllSendersMessageCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #testing }
SycInlineAllSendersMessageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : #activation }
SycInlineAllSendersMessageCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 35
		for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycInlineAllSendersMessageCommand >> createRefactoring [
	|refactoring|
	refactoring := RBInlineAllSendersRefactoring
		sendersOf: originalMessage selector 
		in: originalMessage contextUser origin.
	refactoring setOption: #inlineExpression toUse: [ :ref :aString | self confirm: ('Do you want to inline the expression ''<1s>'' in the current method?' expandMacrosWith: aString) ] .
	^ refactoring 
]

{ #category : #accessing }
SycInlineAllSendersMessageCommand >> defaultMenuIconName [
	^#smallRightFlushIcon

]

{ #category : #accessing }
SycInlineAllSendersMessageCommand >> defaultMenuItemName [
	^'Inline senders'
]
