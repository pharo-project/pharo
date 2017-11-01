"
A MCMerger is an abstract responsible for performing merge operations and detecting conflicts.
"
Class {
	#name : #MCMerger,
	#superclass : #Object,
	#category : 'Monticello-Merging'
}

{ #category : #operations }
MCMerger >> addConflictWithOperation: anOperation [
	self operations add: anOperation beConflict
]

{ #category : #operations }
MCMerger >> applyTo: anObject [
	self isMerged ifFalse: [self error: 'You must resolve all the conflicts first'].
	self operations do: [:ea | ea applyTo: anObject]
]

{ #category : #accessing }
MCMerger >> conflicts [	
	^ self operations select: #isConflict

]

{ #category : #testing }
MCMerger >> hasConflicts [
	^ self conflicts notEmpty
]

{ #category : #testing }
MCMerger >> isMerged [
	^ self conflicts allSatisfy: [:ea | ea isResolved]
]

{ #category : #operations }
MCMerger >> load [
	| loader |
	loader := MCPackageLoader new.
	loader provisions addAll: self provisions.
	self applyTo: loader.
	loader load
]

{ #category : #operations }
MCMerger >> loadWithNameLike: baseName [
	| loader |
	loader := MCPackageLoader new.
	loader provisions addAll: self provisions.
	self applyTo: loader.
	loader loadWithNameLike: baseName
]

{ #category : #accessing }
MCMerger >> mergedSnapshot [
	^ MCPatcher apply: self to: self baseSnapshot
]

{ #category : #accessing }
MCMerger >> operations [
	^ #()
]

{ #category : #accessing }
MCMerger >> provisions [
	^ #()
]
