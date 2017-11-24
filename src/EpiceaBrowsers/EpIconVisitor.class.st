"
I answer an icon for each EpEvent.
"
Class {
	#name : #EpIconVisitor,
	#superclass : #EpEntryContentVisitor,
	#category : #'EpiceaBrowsers-Visitors'
}

{ #category : #private }
EpIconVisitor >> additionIcon [
	^ self iconNamed: #changeAddIcon
]

{ #category : #private }
EpIconVisitor >> emptyIcon [
	^ self iconNamed: #emptyIcon
]

{ #category : #private }
EpIconVisitor >> modificationIcon [
	^ self iconNamed: #changeUpdateIcon
]

{ #category : #private }
EpIconVisitor >> removalIcon [
	^ self iconNamed: #changeRemoveIcon
]

{ #category : #visitor }
EpIconVisitor >> visitBehaviorCategoryChange: aClassRecategorizationChange [
	^ self modificationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitBehaviorCommentChange: aChange [
	^ self modificationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitBehaviorCopy: anEvent [
	^ self iconNamed: #configurationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitBehaviorNameChange: aClassRenamedChange [
	^ self modificationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitCategoryAddition: aCategoryChange [
	^ self additionIcon
]

{ #category : #visitor }
EpIconVisitor >> visitCategoryRemoval: aCategoryChange [
	^ self removalIcon
]

{ #category : #visitor }
EpIconVisitor >> visitCategoryRename: aCategoryChange [
	^ self modificationIcon.
]

{ #category : #visitor }
EpIconVisitor >> visitClassAddition: aClassCreatedChange [
	^ self additionIcon
]

{ #category : #visitor }
EpIconVisitor >> visitClassModification: aModificationClassChange [
	^ self modificationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitClassRemoval: aClassRemovedChange [
	^ self removalIcon
]

{ #category : #visitor }
EpIconVisitor >> visitEvent: anEvent [ 
	^ self emptyIcon
]

{ #category : #visitor }
EpIconVisitor >> visitExpressionEvaluation: anExpressionEvaluation [
	^ self iconNamed: #workspaceIcon
]

{ #category : #visitor }
EpIconVisitor >> visitLogCommentModification: aLogCommentModification [
	^ self iconNamed: #announcementIcon
]

{ #category : #visitor }
EpIconVisitor >> visitMethodAddition: aMethodCreatedChange [
	^ self additionIcon
]

{ #category : #visitor }
EpIconVisitor >> visitMethodModification: aMethodModified [
	^ self modificationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitMethodRemoval: aMethodRemoved [
	^ self removalIcon
]

{ #category : #visitor }
EpIconVisitor >> visitMonticelloVersionSave: aChange [
	^ self iconNamed: #packageIcon
]

{ #category : #visitor }
EpIconVisitor >> visitMonticelloVersionsLoad: aMonticelloVersionLoaded [
	^ self iconNamed: #packageIcon
]

{ #category : #visitor }
EpIconVisitor >> visitProtocolAddition: aProtocolChange [
	^ self additionIcon
]

{ #category : #visitor }
EpIconVisitor >> visitProtocolRemoval: aProtocolChange [
	^ self removalIcon
]

{ #category : #visitor }
EpIconVisitor >> visitRedo: aRedoEvent [
	^ self iconNamed: #smallRedoIcon
]

{ #category : #visitor }
EpIconVisitor >> visitRefactoring: aRefactoring [
	^ self iconNamed: #configurationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitSessionEnd: anEvent [
	^ self iconNamed: #smallQuitIcon
]

{ #category : #visitor }
EpIconVisitor >> visitSessionSnapshot: aSnapshotChange [
	^ self iconNamed: #smallSaveIcon
]

{ #category : #visitor }
EpIconVisitor >> visitSessionStart: anEvent [
	^ self iconNamed: #smallDoItIcon
]

{ #category : #visitor }
EpIconVisitor >> visitTraitAddition: aTraitCreation [
	^ self additionIcon
]

{ #category : #visitor }
EpIconVisitor >> visitTraitModification: aTraitModificationChange [
	^ self modificationIcon
]

{ #category : #visitor }
EpIconVisitor >> visitTraitRemoval:  aTraitRemoval [
	^ self removalIcon
]

{ #category : #visitor }
EpIconVisitor >> visitUndo: anUndoEvent [
	^ self iconNamed: #smallUndoIcon
]
