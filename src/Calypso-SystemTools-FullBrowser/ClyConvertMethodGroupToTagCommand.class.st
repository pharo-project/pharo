"
I am a command to convert given method groups to method tag.
I perform kind of ""Move to protocol"" operation for all contained methods
 
Internal Representation and Key Implementation Points.

    Instance Variables
	tagName:		<Symbol>
"
Class {
	#name : #ClyConvertMethodGroupToTagCommand,
	#superclass : #ClyMethodGroupCommand,
	#instVars : [
		'tagName'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #testing }
ClyConvertMethodGroupToTagCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isMethodTagSelected not
]

{ #category : #activation }
ClyConvertMethodGroupToTagCommand class >> fullBrowserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyMethodGroup asCalypsoItemContext 
]

{ #category : #execution }
ClyConvertMethodGroupToTagCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	aToolContext showMethodTag: tagName
]

{ #category : #accessing }
ClyConvertMethodGroupToTagCommand >> defaultMenuItemName [

	^'Convert to protocol'
]

{ #category : #execution }
ClyConvertMethodGroupToTagCommand >> execute [
	
	methodGroups do: [ :each | each convertToMethodTag: tagName]
]

{ #category : #execution }
ClyConvertMethodGroupToTagCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	tagName := aToolContext requestSingleMethodTag: 'New protocol name'
]

{ #category : #accessing }
ClyConvertMethodGroupToTagCommand >> tagName [
	^ tagName
]

{ #category : #accessing }
ClyConvertMethodGroupToTagCommand >> tagName: anObject [
	tagName := anObject
]
