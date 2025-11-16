Class {
	#name : 'RubMoveDownAction',
	#superclass : 'RubCursorAction',
	#category : 'Rubric-Editing-Core',
	#package : 'Rubric',
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
