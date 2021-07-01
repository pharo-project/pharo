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
		'newSelector'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #testing }
SycAddMessageArgumentCommand class >> canBeExecutedInContext: aToolContext [

	"This method overrides the superclass definition to make appear this option only for methods.

	Using the refactor from the method selection panel works fine.
	Otherwise createValidNameArgument creates an argument name that is not already in use (for example: if anObject is used then it return anObject1).
	However when we ask this refactoring from a message we don't know the method and cannot do those checks. (If there is only one)"
	^aToolContext isMethodSelected
]

{ #category : #activation }
SycAddMessageArgumentCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ SycMethodMenuActivation
		byRootGroupItemOrder: 30 for: ClyMethod asCalypsoItemContext
]

{ #category : #requesting }
SycAddMessageArgumentCommand >> canRemoveArgs [ 
	^ false
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
	
	newSelector := self requestNewMessageIn: aToolContext.
]

{ #category : #execution }
SycAddMessageArgumentCommand >> resultMessageSelector [
	^newSelector
]
