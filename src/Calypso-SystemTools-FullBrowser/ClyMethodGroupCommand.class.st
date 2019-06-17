"
I am a base class for command which perform some action with method groups.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	methodGroups:		<Collection of<ClyMethodGroup>>
"
Class {
	#name : #ClyMethodGroupCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'methodGroups'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #testing }
ClyMethodGroupCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isMethodGroupSelected
]

{ #category : #testing }
ClyMethodGroupCommand class >> isAbstract [
	^self = ClyMethodGroupCommand 
]

{ #category : #accessing }
ClyMethodGroupCommand >> methodGroups [
	^ methodGroups
]

{ #category : #accessing }
ClyMethodGroupCommand >> methodGroups: anObject [
	methodGroups := anObject
]

{ #category : #'activation-drag and drop' }
ClyMethodGroupCommand >> prepareExecutionInDragContext: aToolContext [
	super prepareExecutionInDragContext: aToolContext.
	
	methodGroups := aToolContext selectedMethodGroups
]

{ #category : #execution }
ClyMethodGroupCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	methodGroups := aToolContext selectedMethodGroups
]
