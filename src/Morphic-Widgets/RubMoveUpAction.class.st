Class {
	#name : 'RubMoveUpAction',
	#superclass : 'RubCursorAction',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMoveUpAction >> isForward [
	
	^ false.
]

{ #category : 'movement' }
RubMoveUpAction >> moveFromPosition: position inEditor: editor [

	^ editor sameColumn: position newLine: [ :line | line - 1 ] forward: self isForward
]
