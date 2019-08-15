Class {
	#name : #SycReplaceMessageCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSelector'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #activation }
SycReplaceMessageCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 1100 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycReplaceMessageCommand >> createRefactoring [

	^ RBReplaceMethodRefactoring
		replaceCallMethod: originalMessage selector 
		in: originalMessage contextUser origin
		to: newSelector
		permutation: (1 to: originalMessage argumentNames size)
]

{ #category : #accessing }
SycReplaceMessageCommand >> defaultMenuIconName [
	^#edit
]

{ #category : #accessing }
SycReplaceMessageCommand >> defaultMenuItemName [
	^ 'Replace senders'
]

{ #category : #testing }
SycReplaceMessageCommand >> isComplexRefactoring [
	^true
]

{ #category : #execution }
SycReplaceMessageCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	newSelector := UIManager default 
		request: 'Update senders of ', originalMessage selector, ' with:' initialAnswer: originalMessage selector title: 'Replace senders of method'.
		
	newSelector isEmptyOrNil | (newSelector = originalMessage selector) ifTrue: [ CmdCommandAborted signal]
]
