Class {
	#name : #SycUndoSourceCodeCommand,
	#superclass : #SycSourceCodeCommand,
	#category : #'SystemCommands-SourceCodeCommands'
}

{ #category : #accessing }
SycUndoSourceCodeCommand >> defaultMenuIconName [ 
	^ #edit
]

{ #category : #accessing }
SycUndoSourceCodeCommand >> defaultMenuItemName [
	^'Undo'
]

{ #category : #execution }
SycUndoSourceCodeCommand >> execute [
	[ |limit list| 
	list := OrderedCollection new.
	limit := RBRefactoryChangeManager instance lastUndoPointer .
	1 to: limit do:[ :i |
		list add: RBRefactoryChangeManager instance undoChange ].
	list do: [ :e | e execute ]
	 ] asJob
		title: 'Refactoring';
		run
]
