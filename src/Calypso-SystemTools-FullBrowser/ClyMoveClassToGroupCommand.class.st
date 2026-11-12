"
I am a command to move classes into given class group.

By default I only available by dragging classes into the target class group.

Internal Representation and Key Implementation Points.

    Instance Variables
	classGroup:		<ClyClassGroup>
"
Class {
	#name : 'ClyMoveClassToGroupCommand',
	#superclass : 'SycClassCommand',
	#instVars : [
		'classGroup'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Classes',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Classes'
}

{ #category : 'accessing' }
ClyMoveClassToGroupCommand >> classGroup [
	^ classGroup
]

{ #category : 'accessing' }
ClyMoveClassToGroupCommand >> classGroup: anObject [
	classGroup := anObject
]

{ #category : 'execution' }
ClyMoveClassToGroupCommand >> executeRefactoring [

	classes do: [ :class | classGroup addClass: class ]
]

{ #category : 'accessing' }
ClyMoveClassToGroupCommand >> iconName [
	^ #smallRedo
]

{ #category : 'accessing' }
ClyMoveClassToGroupCommand >> name [
	^'Move to package'
]

{ #category : 'execution' }
ClyMoveClassToGroupCommand >> prepareFullExecution [
	super prepareFullExecution.
	"class group could be package. In this activation it is only make sense to request user for package"
	classGroup := self dropContext
		ifNil: [ self context requestSinglePackage: 'Choose package' ]
		ifNotNil: [ :drop | drop lastSelectedClassGroup ]
]
