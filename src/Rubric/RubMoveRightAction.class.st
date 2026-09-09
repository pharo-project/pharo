Class {
	#name : 'RubMoveRightAction',
	#superclass : 'RubCursorAction',
	#category : 'Rubric-Editing-Core',
	#package : 'Rubric',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMoveRightAction >> isForward [
	
	^ true.
]

{ #category : 'movement' }
RubMoveRightAction >> moveFromPosition: position inEditor: editor [

	editor invalidateVirtualColumn.
	^ cmdPressed 
		  ifTrue: [ self nextWordPosition: position editor: editor ]
		  ifFalse: [ self nextCharacterPosition: position editor: editor ]
]

{ #category : 'movement' }
RubMoveRightAction >> nextCharacterPosition: position editor: editor [

	^ position + 1 min: editor bufferSize + 1
]

{ #category : 'movement' }
RubMoveRightAction >> nextWordPosition: position editor: editor [

	^ editor nextWord: position stopOnUpperCase: optionPressed 
]
