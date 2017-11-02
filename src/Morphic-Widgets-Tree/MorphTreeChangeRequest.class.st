"
Used to announce a change request on the tree
"
Class {
	#name : #MorphTreeChangeRequest,
	#superclass : #MorphTreeAnnounce,
	#instVars : [
		'action',
		'arguments'
	],
	#category : 'Morphic-Widgets-Tree-Announcements'
}

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> chunkSize: anInteger [
	^ self new action: #chunkSize:; arguments: (Array with: anInteger)
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> collapseAll [
	^ self new action: #collapseAll
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> collapseNodePath: aNodePath [
	^ self new action: #collapseNodePath:; arguments: (Array with: aNodePath) 
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> deselectAll [
	^ self new action: #deselectAll
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> enabled [
	^ self new action: #enabledFromModel
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> expandAll [
	^ self new action: #expandAll
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> expandAllFromNode: aNode [
	^ self new action: #expandAllFromNode:; arguments: (Array with: aNode)
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> expandItemPath: anItemPath [
	^ self new action: #expandItemPath:; arguments: (Array with: anItemPath)
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> expandNodePath: aNodePath [
	^ self new action: #expandNodePath:; arguments: (Array with: aNodePath)
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> expandRoots [
	^ self new action: #expandRoots
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> pageSize: anInteger [
	^ self new action: #pageSize:; arguments: (Array with: anInteger)
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> selectAll [
	^ self new action: #selectAll
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> selectItems: itemsList [
	^ self new action: #selectItems:; arguments: (Array with: itemsList)
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> updateList [
	^ self new action: #updateList
]

{ #category : #'instance creation' }
MorphTreeChangeRequest class >> updateSelectionFromModel [
	^ self new action: #updateSelectionFromModel
]

{ #category : #accessing }
MorphTreeChangeRequest >> action: aSymbol [
	action := aSymbol
]

{ #category : #accessing }
MorphTreeChangeRequest >> arguments [
	^ arguments ifNil: [Array new]
]

{ #category : #accessing }
MorphTreeChangeRequest >> arguments: anArray [
	arguments := anArray
]

{ #category : #accessing }
MorphTreeChangeRequest >> change: aTreeMorph [
	action ifNil: [^self].
	aTreeMorph perform: action withEnoughArguments: self arguments
]
