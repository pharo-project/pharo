"
I answer an icon name for each EpEvent.
"
Class {
	#name : 'EpIconNameVisitor',
	#superclass : 'EpEntryContentVisitor',
	#category : 'EpiceaBrowsers-Visitors',
	#package : 'EpiceaBrowsers',
	#tag : 'Visitors'
}

{ #category : 'private' }
EpIconNameVisitor >> additionIcon [
	^ #changeAdd
]

{ #category : 'private' }
EpIconNameVisitor >> emptyIcon [
	^ #empty
]

{ #category : 'private' }
EpIconNameVisitor >> modificationIcon [
	^ #changeUpdate
]

{ #category : 'private' }
EpIconNameVisitor >> removalIcon [
	^ #changeRemove
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitBehaviorCommentChange: aChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitBehaviorCopy: anEvent [
	^ #configuration
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitBehaviorNameChange: aClassRenamedChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitBehaviorRepackagedChange: aBehaviorRepackagedChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitClassAddition: aClassCreatedChange [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitClassModification: aModificationClassChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitClassRemoval: aClassRemovedChange [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitEvent: anEvent [
	^ self emptyIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitExpressionEvaluation: anExpressionEvaluation [
	^ #workspace
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitLogCommentModification: aLogCommentModification [
	^ #announcement
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitMethodAddition: aMethodCreatedChange [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitMethodModification: aMethodModified [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitMethodRemoval: aMethodRemoved [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitMonticelloVersionSave: aChange [
	^ #package
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitMonticelloVersionsLoad: aMonticelloVersionLoaded [
	^ #package
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitPackageAddition: aPackageAddition [

	^ self additionIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitPackageRemoval: aPackageRemoval [

	^ self removalIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitPackageRename: aPackageRename [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitPackageTagAddition: aPackageTagAddition [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitPackageTagRemoval: aPackageTagRemoval [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitPackageTagRename: aPackageTagRename [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitProtocolAddition: aProtocolChange [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitProtocolRemoval: aProtocolChange [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitRedo: aRedoEvent [
	^ #smallRedo
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitRefactoring: aRefactoring [
	^ #configuration
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitSessionEnd: anEvent [
	^ #smallQuit
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitSessionSnapshot: aSnapshotChange [
	^ #smallSave
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitSessionStart: anEvent [
	^ #smallDoIt
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitTraitAddition: aTraitCreation [
	^ self additionIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitTraitModification: aTraitModificationChange [
	^ self modificationIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitTraitRemoval:  aTraitRemoval [
	^ self removalIcon
]

{ #category : 'visitor' }
EpIconNameVisitor >> visitUndo: anUndoEvent [
	^ #smallUndo
]
