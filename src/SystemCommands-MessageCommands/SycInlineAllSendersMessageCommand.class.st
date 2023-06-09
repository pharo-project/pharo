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

{ #category : #execution }
SycInlineAllSendersMessageCommand >> createRefactoring [

	| refactoring methodOrigin |
	"Extract the method origin from the context.
	The contextUser can be
	  - a method (if selected from the method list)
	  - an AST (if selected from the text editor).
	Ask #method to resolve to the method, and then ask the origin, i.e. the class or trait where it is defined"
	methodOrigin := originalMessage contextUser compiledMethod origin.
	refactoring := RBInlineAllSendersRefactoring model: model sendersOf: originalMessage selector in: methodOrigin.
	refactoring setOption: #inlineExpression toUse: [ :ref :aString |
		(self confirm: ('Do you want to extract the expression ''<1s>'' into a variable in method ''<2s>''?' expandMacrosWith: aString with: ref sourceSelector)) not ].
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
