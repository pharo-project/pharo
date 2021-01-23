Extension { #name : #AbstractTool }

{ #category : #'*Refactoring-Changes' }
AbstractTool class >> undoLastRefactoring [
	| manager |
	manager := RBRefactoryChangeManager instance.
	manager undoPointers ifNotEmpty: [
	[ |limit list| 
	list := OrderedCollection new.
	limit := manager lastUndoPointer .
	1 to: limit do:[ :i | list add: manager undoChange ].
	list do: [ :e | e execute ]
	 ] asJob
		title: 'Refactoring';
		run]
	ifEmpty: [ self inform: 'There aren''t refactorings to undo.'  ]
]
