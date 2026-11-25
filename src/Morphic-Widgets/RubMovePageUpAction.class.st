Class {
	#name : 'RubMovePageUpAction',
	#superclass : 'RubCursorAction',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMovePageUpAction >> isForward [
	
	^ false 
]

{ #category : 'movement' }
RubMovePageUpAction >> moveFromPosition: position inEditor: editor [

	editor invalidateVirtualColumn.
	^ editor sameColumn: position newLine: [ :lineNo | lineNo - editor pageHeight ] forward: self isForward
]
