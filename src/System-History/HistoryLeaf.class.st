"
This class represents the abstract leaf structure of items stored in History tree. Composite nodes of such trees are made of HistoryNode instances. For a particular use, this class need to be subclassed. See for example UndoRedoRecord
"
Class {
	#name : #HistoryLeaf,
	#superclass : #Object,
	#category : #'System-History-Utilities'
}

{ #category : #adding }
HistoryLeaf >> addItem: anHistoryItem [ 
	^ false
]

{ #category : #testing }
HistoryLeaf >> isComposite [
	^ false
]

{ #category : #testing }
HistoryLeaf >> opened [
	^ false
]
