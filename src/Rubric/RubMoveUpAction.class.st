Class {
	#name : 'RubMoveUpAction',
	#superclass : 'RubCursorAction',
	#category : 'Rubric-Editing-Core',
	#package : 'Rubric',
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
