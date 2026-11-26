Class {
	#name : 'RubMovePageDownAction',
	#superclass : 'RubCursorAction',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMovePageDownAction >> isForward [
	
	^ true 
]

{ #category : 'movement' }
RubMovePageDownAction >> moveFromPosition: position inEditor: editor [

	editor invalidateVirtualColumn.
	^ editor sameColumn: position newLine: [ :lineNo | lineNo + editor pageHeight ] forward: self isForward
]
