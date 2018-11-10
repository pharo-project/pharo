"
I'm a command to install a chunk of code from a commit (shown in a diff) into the image. 
""chunk"" can be a package, class or single method.
"
Class {
	#name : #IceTipRevertChangeCommand,
	#superclass : #IceTipCommand,
	#instVars : [
		'item',
		'announcer'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #activation }
IceTipRevertChangeCommand class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemFor: IceTipDiffContext
]

{ #category : #accessing }
IceTipRevertChangeCommand class >> defaultHelp [
	^ 'Revert selected change in the image'
]

{ #category : #accessing }
IceTipRevertChangeCommand class >> defaultMenuIconName [
	^ #smallUndo
]

{ #category : #accessing }
IceTipRevertChangeCommand class >> defaultMenuItemName [
	^ 'Revert change'
]

{ #category : #execution }
IceTipRevertChangeCommand >> announcer [
	^ announcer ifNotNil: #contents
]

{ #category : #execution }
IceTipRevertChangeCommand >> execute [
	UIManager default 
		informUser: 'Reverting change...' 
		during: [ self repositoryModel entity workingCopy discardChanges: item ].
	self announcer announce: (IceTipDiffEntryRemoved entry: item).
	Iceberg announcer announce: (IceRepositoryModified for: self repositoryModel entity)
]

{ #category : #execution }
IceTipRevertChangeCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	item := aToolContext item.
	announcer := IceTipWeakValueHolder contents: aToolContext toolAnnouncer
]
