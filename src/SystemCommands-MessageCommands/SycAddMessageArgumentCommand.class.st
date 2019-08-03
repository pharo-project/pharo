"
I am a command to add new argument into the message.

Internal Representation and Key Implementation Points.

    Instance Variables
	argumentDefaultValue:		<Object>
	newSelector:		<Symbol>
"
Class {
	#name : #SycAddMessageArgumentCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'newSelector',
		'argumentDefaultValue'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #activation }
SycAddMessageArgumentCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 30 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
SycAddMessageArgumentCommand >> argumentDefaultValue [
	^ argumentDefaultValue
]

{ #category : #accessing }
SycAddMessageArgumentCommand >> argumentDefaultValue: anObject [
	argumentDefaultValue := anObject
]

{ #category : #execution }
SycAddMessageArgumentCommand >> createRefactoring [

	^RBAddParameterRefactoring
		addParameterToMethod: originalMessage selector
		in: (SycMessageOriginHack of: originalMessage) "look at SycMessageOriginHack comment"
		newSelector: newSelector
		initializer: argumentDefaultValue
]

{ #category : #accessing }
SycAddMessageArgumentCommand >> defaultMenuIconName [
	^#smallAdd

]

{ #category : #accessing }
SycAddMessageArgumentCommand >> defaultMenuItemName [
	^'Add argument'
]

{ #category : #accessing }
SycAddMessageArgumentCommand >> newSelector [
	^ newSelector
]

{ #category : #accessing }
SycAddMessageArgumentCommand >> newSelector: anObject [
	newSelector := anObject
]

{ #category : #execution }
SycAddMessageArgumentCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	newSelector := UIManager default
		request: 'Enter new selector:' initialAnswer: originalMessage selector.
	newSelector isEmptyOrNil ifTrue:  [ CmdCommandAborted signal ].
	
	argumentDefaultValue := UIManager default
		request: 'Enter default value for argument' initialAnswer: 'nil'.
	argumentDefaultValue isEmptyOrNil ifTrue:  [ CmdCommandAborted signal ]
]

{ #category : #execution }
SycAddMessageArgumentCommand >> resultMessageSelector [
	^newSelector
]
