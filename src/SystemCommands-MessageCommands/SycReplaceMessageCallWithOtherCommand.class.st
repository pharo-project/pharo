Class {
	#name : #SycReplaceMessageCallWithOtherCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSelector'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #activation }
SycReplaceMessageCallWithOtherCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 1100 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycReplaceMessageCallWithOtherCommand >> createRefactoring [

	^ RBReplaceCalledMethodByAnotherRefactoring
		replaceCallMethod: originalMessage selector 
		in: originalMessage contextUser origin
		to: newSelector
		permutation: (1 to: originalMessage argumentNames size)
]

{ #category : #accessing }
SycReplaceMessageCallWithOtherCommand >> defaultMenuIconName [
	^#edit
]

{ #category : #accessing }
SycReplaceMessageCallWithOtherCommand >> defaultMenuItemName [
	^ 'Replace senders'
]

{ #category : #testing }
SycReplaceMessageCallWithOtherCommand >> isComplexRefactoring [
	^true
]

{ #category : #execution }
SycReplaceMessageCallWithOtherCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	newSelector := UIManager default 
		request: 'Update senders of ', originalMessage selector, ' with:' initialAnswer: originalMessage selector title: 'Replace senders of method'.
		
	newSelector isEmptyOrNil | (newSelector = originalMessage selector) ifTrue: [ CmdCommandAborted signal]
]
