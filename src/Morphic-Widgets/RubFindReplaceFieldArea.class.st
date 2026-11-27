"
I am the text area of a field of the inline find and replace bar (see RubFindReplaceField).

I give the keys that drive the search to the bar (see RubFindReplaceBar>>handleKeyDown:from:) before handling them as edits, and Cmd+F in me selects the find text instead of opening another bar.
"
Class {
	#name : 'RubFindReplaceFieldArea',
	#superclass : 'RubTextFieldArea',
	#category : 'Morphic-Widgets-Editing-FindReplaceService',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'accessing' }
RubFindReplaceFieldArea >> findBar [

	^ self ownerThatIsA: RubFindReplaceBar
]

{ #category : 'event handling' }
RubFindReplaceFieldArea >> keyDown: anEvent [

	(self findBar ifNil: [ false ] ifNotNil: [ :bar | bar handleKeyDown: anEvent from: self ]) ifTrue: [ ^ self ].
	super keyDown: anEvent
]

{ #category : 'find-replace' }
RubFindReplaceFieldArea >> openFindDialog [

	self findBar ifNotNil: [ :bar | bar focusFindField ]
]
