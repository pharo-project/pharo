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
	
	^ SycMethodMenuActivation
		byRootGroupItemOrder: 30 for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
SycAddMessageArgumentCommand >> argumentDefaultValue [
	^ argumentDefaultValue
]

{ #category : #accessing }
SycAddMessageArgumentCommand >> argumentDefaultValue: anObject [
	argumentDefaultValue := anObject
]

{ #category : #actions }
SycAddMessageArgumentCommand >> computeInvalidArgNames [ 
	| invalidArgNames |
	invalidArgNames := Set new.
	(model allImplementorsOf: originalMessage selector) do: [ :e | | cm | 
		cm:=(e realClass >> originalMessage selector).
		invalidArgNames addAll: cm argumentNames;
			addAll: cm origin instVarNames ].
	^ invalidArgNames
]

{ #category : #execution }
SycAddMessageArgumentCommand >> createRefactoring [

	^(RBAddParameterRefactoring
		model: model
		addParameterToMethod: originalMessage selector
		in: (SycMessageOriginHack of: originalMessage) "look at SycMessageOriginHack comment"
		newSelector: newSelector selector
		permutation: newSelector permutation
		newArgs: newSelector newArgs)
		renameMap: newSelector renameMap
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
	
	newSelector := self requestMessageIn: aToolContext.
]

{ #category : #execution }
SycAddMessageArgumentCommand >> requestMessageIn: aToolContext [
	| methodName dialog invalidArgNames |
	invalidArgNames := self computeInvalidArgNames.
	methodName := RBMethodName selector: originalMessage selector arguments: originalMessage argumentNames.
	dialog := SycMethodNameEditorPresenter openOn: methodName withInvalidArgs: invalidArgNames
		canRenameArgs: true canRemoveArgs: false canAddArgs: true.
	dialog cancelled ifTrue: [  CmdCommandAborted signal ].
	
	originalMessage selector = methodName selector & (originalMessage argumentNames = methodName arguments)
		ifTrue: [ CmdCommandAborted signal].	
	^methodName
]

{ #category : #execution }
SycAddMessageArgumentCommand >> resultMessageSelector [
	^newSelector
]
