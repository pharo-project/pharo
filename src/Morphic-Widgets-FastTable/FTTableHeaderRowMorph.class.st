"
I'm a header for a table.
All my work is to keep header cells, but my behavior is slightly different to my parent (since I cannot be selected, for example)
"
Class {
	#name : #FTTableHeaderRowMorph,
	#superclass : #FTTableRowMorph,
	#category : #'Morphic-Widgets-FastTable'
}

{ #category : #'event handling' }
FTTableHeaderRowMorph >> handlesMouseOver: event [

	^ false
]
