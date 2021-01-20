"
I am a command to remove existing argument from the message.

Internal Representation and Key Implementation Points.

    Instance Variables
	argumentName:		<String>
"
Class {
	#name : #SycRemoveMessageArgumentCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#instVars : [
		'argumentName',
		'newSignature'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #testing }
SycRemoveMessageArgumentCommand class >> canBeExecutedInContext: aToolContext [
	^(super canBeExecutedInContext: aToolContext)
		and: [ aToolContext lastSelectedMessage arguments notEmpty]
]

{ #category : #activation }
SycRemoveMessageArgumentCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 40
		for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
SycRemoveMessageArgumentCommand >> argumentName [
	^ argumentName
]

{ #category : #accessing }
SycRemoveMessageArgumentCommand >> argumentName: anObject [
	argumentName := anObject
]

{ #category : #execution }
SycRemoveMessageArgumentCommand >> createRefactoring [

	^RBRemoveParameterRefactoring
		removeParameter: argumentName
		in: class
		selector: originalMessage selector
]

{ #category : #accessing }
SycRemoveMessageArgumentCommand >> defaultMenuIconName [ 
	^#remove

]

{ #category : #accessing }
SycRemoveMessageArgumentCommand >> defaultMenuItemName [
	^'Remove argument'
]

{ #category : #execution }
SycRemoveMessageArgumentCommand >> prepareFullExecutionInContext: aToolContext [
	| parseTree |
	super prepareFullExecutionInContext: aToolContext.
	parseTree := class parseTreeFor: originalMessage selector.
	argumentName := originalMessage arguments size = 1 
		ifTrue: [ parseTree argumentNames first]
		ifFalse: [ UIManager default
						chooseFrom: parseTree argumentNames 
						values: parseTree argumentNames 
						message: 'Choose an argument to remove:'].
					
	(parseTree argumentNames includes: argumentName) 
		ifFalse: [ CmdCommandAborted signal ].
]

{ #category : #execution }
SycRemoveMessageArgumentCommand >> resultMessageSelector [
	| argIndex keywords |
	argIndex := originalMessage argumentNames indexOf: argumentName.
	keywords := originalMessage selector keywords asOrderedCollection.
	keywords size = 1
		ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: argIndex.
	^(keywords joinUsing: '') asSymbol
]
