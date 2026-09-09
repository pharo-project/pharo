Class {
	#name : 'RubMovePageDownAction',
	#superclass : 'RubCursorAction',
	#category : 'Rubric-Editing-Core',
	#package : 'Rubric',
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
