"
I am a single line text field of the inline find and replace bar (see RubFindReplaceBar): the find field or the replace field.

My text area (RubFindReplaceFieldArea) gives the keys that drive the search (Enter, Shift+Enter, Esc) to the bar before editing.
"
Class {
	#name : 'RubFindReplaceField',
	#superclass : 'RubTextFieldMorph',
	#category : 'Morphic-Widgets-Editing-FindReplaceService',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'initialization' }
RubFindReplaceField >> textAreaClass [

	^ RubFindReplaceFieldArea
]
