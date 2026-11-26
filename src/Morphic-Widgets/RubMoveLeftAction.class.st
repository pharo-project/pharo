Class {
	#name : 'RubMoveLeftAction',
	#superclass : 'RubCursorAction',
	#category : 'Morphic-Widgets-Editing-Core',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMoveLeftAction >> isForward [
	
	^ false.
]

{ #category : 'movement' }
RubMoveLeftAction >> moveFromPosition: position inEditor: editor [

	editor invalidateVirtualColumn.
	^ cmdPressed 
		  ifTrue: [ self previousWordPosition: position editor: editor ]
		  ifFalse: [ self previousCharacterPosition: position editor: editor ]
]

{ #category : 'movement' }
RubMoveLeftAction >> previousCharacterPosition: position editor: editor [
	
	^ position - 1 max: 1
]

{ #category : 'movement' }
RubMoveLeftAction >> previousWordPosition: position editor: editor [
	
	^ editor previousWord: position stopOnUpperCase: optionPressed
]
