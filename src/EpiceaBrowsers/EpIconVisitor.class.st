"
I answer an icon for each EpEvent.
"
Class {
	#name : 'EpIconVisitor',
	#superclass : 'EpEntryContentVisitor',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'private' }
EpIconVisitor >> additionIcon [
	^ self iconNamed: #changeAdd
]

{ #category : 'private' }
EpIconVisitor >> emptyIcon [
	^ self iconNamed: #empty
]

{ #category : 'private' }
EpIconVisitor >> modificationIcon [
	^ self iconNamed: #changeUpdate
]

{ #category : 'private' }
EpIconVisitor >> removalIcon [
	^ self iconNamed: #changeRemove
]

{ #category : 'visitor' }
EpIconVisitor >> visitBehaviorCommentChange: aChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitBehaviorCopy: anEvent [
	^ self iconNamed: #configuration
]

{ #category : 'visitor' }
EpIconVisitor >> visitBehaviorNameChange: aClassRenamedChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitClassAddition: aClassCreatedChange [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitClassModification: aModificationClassChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitClassRemoval: aClassRemovedChange [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitEvent: anEvent [
	^ self empty
]

{ #category : 'visitor' }
EpIconVisitor >> visitExpressionEvaluation: anExpressionEvaluation [
	^ self iconNamed: #workspace
]

{ #category : 'visitor' }
EpIconVisitor >> visitLogCommentModification: aLogCommentModification [
	^ self iconNamed: #announcement
]

{ #category : 'visitor' }
EpIconVisitor >> visitMethodAddition: aMethodCreatedChange [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitMethodModification: aMethodModified [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitMethodRemoval: aMethodRemoved [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitMonticelloVersionSave: aChange [
	^ self iconNamed: #package
]

{ #category : 'visitor' }
EpIconVisitor >> visitMonticelloVersionsLoad: aMonticelloVersionLoaded [
	^ self iconNamed: #package
]

{ #category : 'visitor' }
EpIconVisitor >> visitPackageAddition: aPackageAddition [

	^ self additionIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitPackageRemoval: aPackageRemoval [

	^ self removalIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitPackageRename: aPackageRename [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitPackageTagAddition: aPackageTagAddition [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitPackageTagRemoval: aPackageTagRemoval [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitPackageTagRename: aPackageTagRename [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitProtocolAddition: aProtocolChange [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitProtocolRemoval: aProtocolChange [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitRedo: aRedoEvent [
	^ self iconNamed: #smallRedo
]

{ #category : 'visitor' }
EpIconVisitor >> visitRefactoring: aRefactoring [
	^ self iconNamed: #configuration
]

{ #category : 'visitor' }
EpIconVisitor >> visitSessionEnd: anEvent [
	^ self iconNamed: #smallQuit
]

{ #category : 'visitor' }
EpIconVisitor >> visitSessionSnapshot: aSnapshotChange [
	^ self iconNamed: #smallSave
]

{ #category : 'visitor' }
EpIconVisitor >> visitSessionStart: anEvent [
	^ self iconNamed: #smallDoIt
]

{ #category : 'visitor' }
EpIconVisitor >> visitTraitAddition: aTraitCreation [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitTraitModification: aTraitModificationChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitTraitRemoval:  aTraitRemoval [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconVisitor >> visitUndo: anUndoEvent [
	^ self iconNamed: #smallUndo
]
