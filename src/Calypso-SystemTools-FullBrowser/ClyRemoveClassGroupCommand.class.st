"
I am a command to remove class group with containing classes.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	classGroup:		<ClyClassGroup>
"
Class {
	#name : #ClyRemoveClassGroupCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'classGroup'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Class groups'
}

{ #category : #testing }
ClyRemoveClassGroupCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassTagSelected
]

{ #category : #activation }
ClyRemoveClassGroupCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 10000 for: ClyClassGroupContextOfFullBrowser 
]

{ #category : #activation }
ClyRemoveClassGroupCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation removalFor: ClyClassGroupContextOfFullBrowser 
]

{ #category : #accessing }
ClyRemoveClassGroupCommand >> classGroup [
	^ classGroup
]

{ #category : #accessing }
ClyRemoveClassGroupCommand >> classGroup: anObject [
	classGroup := anObject
]

{ #category : #accessing }
ClyRemoveClassGroupCommand >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #accessing }
ClyRemoveClassGroupCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : #execution }
ClyRemoveClassGroupCommand >> execute [
	
	classGroup removeWithClasses
]

{ #category : #execution }
ClyRemoveClassGroupCommand >> prepareFullExecutionInContext: aToolContext [
	| confirmed |
	super prepareFullExecutionInContext: aToolContext.
	
	classGroup := aToolContext lastSelectedClassGroup.

	classGroup classesSize > 0 ifTrue: [
		confirmed := UIManager default 
			confirm: 'Are you sure you want to remove all classes in group ', classGroup name, '?'.
		confirmed ifFalse: [CmdCommandAborted signal]]
]
