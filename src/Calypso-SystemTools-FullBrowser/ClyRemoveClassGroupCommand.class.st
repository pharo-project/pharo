"
I am a command to remove class group with containing classes.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	classGroup:		<ClyClassGroup>
"
Class {
	#name : 'ClyRemoveClassGroupCommand',
	#superclass : 'SycPackageCommand',
	#instVars : [
		'classGroup'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Class groups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Class groups'
}

{ #category : 'testing' }
ClyRemoveClassGroupCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isPackageTagSelected
]

{ #category : 'activation' }
ClyRemoveClassGroupCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation removalFor: ClyFullBrowserClassGroupContext
]

{ #category : 'accessing' }
ClyRemoveClassGroupCommand >> classGroup [
	^ classGroup
]

{ #category : 'accessing' }
ClyRemoveClassGroupCommand >> classGroup: anObject [
	classGroup := anObject
]

{ #category : 'accessing' }
ClyRemoveClassGroupCommand >> description [

^ 'Removes class group with containing classes.'
]

{ #category : 'execution' }
ClyRemoveClassGroupCommand >> executeRefactoring [

	classGroup removeWithClasses
]

{ #category : 'accessing' }
ClyRemoveClassGroupCommand >> iconName [
	^#remove
]

{ #category : 'testing' }
ClyRemoveClassGroupCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassGroupContext
]

{ #category : 'accessing' }
ClyRemoveClassGroupCommand >> name [
	^'Remove'
]

{ #category : 'accessing' }
ClyRemoveClassGroupCommand >> order [

	^ 10000
]

{ #category : 'execution' }
ClyRemoveClassGroupCommand >> prepareFullExecution [

	| confirmed |
	super prepareFullExecution.
	classGroup := self context lastSelectedClassGroup.
	classGroup classesSize > 0 ifTrue: [
			confirmed := self confirm: 'Are you sure you want to remove all classes in group ' , classGroup name , '?'.
			confirmed ifFalse: [ CmCommandAborted signal ] ]
]
