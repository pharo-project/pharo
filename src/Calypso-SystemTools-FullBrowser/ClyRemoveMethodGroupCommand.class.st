"
I am a command to remove method group with containing methods
"
Class {
	#name : 'ClyRemoveMethodGroupCommand',
	#superclass : 'ClyMethodGroupCommand',
	#instVars : [
		'selectedClasses'
	],
	#category : 'Calypso-SystemTools-FullBrowser-Commands-MethodGroups',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-MethodGroups'
}

{ #category : 'activation' }
ClyRemoveMethodGroupCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ClyShortcutActivation removalFor: ClyFullBrowserMethodGroupContext
]

{ #category : 'accessing' }
ClyRemoveMethodGroupCommand >> description [

^ 'Removes method group with containing methods.'
]

{ #category : 'execution' }
ClyRemoveMethodGroupCommand >> executeRefactoring [

	methodGroups do: [ :group |
			group
				removeWithMethods;
				removeProtocolFrom: selectedClasses ]
]

{ #category : 'accessing' }
ClyRemoveMethodGroupCommand >> iconName [
	^#remove
]

{ #category : 'accessing' }
ClyRemoveMethodGroupCommand >> name [
	^'Remove'
]

{ #category : 'accessing' }
ClyRemoveMethodGroupCommand >> order [

	^ 100000
]

{ #category : 'execution' }
ClyRemoveMethodGroupCommand >> prepareFullExecution [

	| tagsString confirmed |
	super prepareFullExecution.
	selectedClasses := self context selectedClasses.
	tagsString := ', ' join: (methodGroups collect: [ :each | each name ]).
	(methodGroups anySatisfy: [ :each | each methodsSize > 0 ]) ifFalse: [ ^ self ].
	confirmed := self confirm: 'Are you sure you want to remove methods from ' , tagsString , '?'.
	confirmed ifFalse: [ CmCommandAborted signal ]
]
