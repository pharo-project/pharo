Class {
	#name : 'RubMoveHomeAction',
	#superclass : 'RubCursorAction',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMoveHomeAction >> beginningOfLine: position editor: editor [

	^ (editor string lastIndexOf: Character cr startingAt: position - 1 ifAbsent: [ 0 ]) + 1
]

{ #category : 'movement' }
RubMoveHomeAction >> isForward [
	
	^ false 
]

{ #category : 'movement' }
RubMoveHomeAction >> moveFromPosition: position inEditor: editor [

	editor invalidateVirtualColumn.
	^ cmdPressed
		  ifTrue: [ 1 ]
		  ifFalse: [ self beginningOfLine: position editor: editor ]
]
