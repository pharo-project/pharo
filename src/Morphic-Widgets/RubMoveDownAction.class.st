Class {
	#name : 'RubMoveDownAction',
	#superclass : 'RubCursorAction',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMoveDownAction >> isForward [
	
	^ true.
]

{ #category : 'movement' }
RubMoveDownAction >> moveFromPosition: position inEditor: editor [

	^ editor sameColumn: position newLine: [ :line | line + 1 ] forward: self isForward
]
