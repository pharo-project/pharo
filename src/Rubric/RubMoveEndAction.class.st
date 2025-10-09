Class {
	#name : 'RubMoveEndAction',
	#superclass : 'RubCursorAction',
	#category : 'Rubric-Editing-Core',
	#package : 'Rubric',
	#tag : 'Editing-Core'
}

{ #category : 'movement' }
RubMoveEndAction >> endOfBuffer: editor [

	^ editor string size + 1
]

{ #category : 'movement' }
RubMoveEndAction >> endOfLine: position editor: editor [

	| string |

	string := editor string.
	^ string indexOf: Character cr startingAt: position ifAbsent: [ string size + 1 ]
]

{ #category : 'movement' }
RubMoveEndAction >> isForward [
	
	^ true
]

{ #category : 'movement' }
RubMoveEndAction >> moveFromPosition: position inEditor: editor [

	editor invalidateVirtualColumn.
	^ cmdPressed
		  ifTrue: [ self endOfBuffer: editor ]
		  ifFalse: [ self endOfLine: position editor: editor ]
]
