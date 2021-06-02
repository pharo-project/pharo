"
I am a command to depracate a message selector.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newSelector:		<ByteSymbol>
"
Class {
	#name : #SycDeprecateMessageCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSelector'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #accessing }
SycDeprecateMessageCommand class >> browserMenuOrder [
	^ 2.5
]

{ #category : #testing }
SycDeprecateMessageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : #activation }
SycDeprecateMessageCommand class >> methodMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation 
		byRootGroupItemOrder: self browserMenuOrder for: ClyMethod asCalypsoItemContext 
]

{ #category : #execution }
SycDeprecateMessageCommand >> createRefactoring [

	^ RBDeprecateMethodRefactoring
		model: model 
		deprecateMethod: originalMessage selector  
		in: originalMessage contextUser origin
		using: newSelector
]

{ #category : #accessing }
SycDeprecateMessageCommand >> defaultMenuItemName [
	^'Deprecate'
]

{ #category : #execution }
SycDeprecateMessageCommand >> prepareFullExecutionInContext: aToolContext [
	| oldSelector |
	super prepareFullExecutionInContext: aToolContext.
	
	oldSelector := originalMessage selector.
	newSelector := UIManager default 
		request: 'Method to replace ', oldSelector  
		initialAnswer: oldSelector
		title: 'New selector'.
	newSelector isEmptyOrNil | (newSelector = oldSelector) ifTrue: [ CmdCommandAborted signal ]
]
