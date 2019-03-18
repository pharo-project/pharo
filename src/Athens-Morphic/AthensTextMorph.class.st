"
this class is subject of changes/removal. do not use it.
"
Class {
	#name : #AthensTextMorph,
	#superclass : #TextMorphForEditView,
	#category : #'Athens-Morphic'
}

{ #category : #operations }
AthensTextMorph >> contents: stringOrText wrappedTo: width [
	"Accept new text contents.  Lay it out, wrapping to width.
	Then fit my height to the result."
	wrapFlag := true.
	super extent: width truncated@self height.
	self newContents: stringOrText
]

{ #category : #operations }
AthensTextMorph >> createParagraph [
	| newParagraph |
	self setDefaultContentsIfNil.

	"...Code here to recreate the paragraph..."
	newParagraph  := AthensParagraph new.
	
	newParagraph
		compose: text
		style: textStyle copy
		in: self container.
		
		
	wrapFlag ifFalse:
		["Was given huge container at first... now adjust"
		newParagraph adjustRightX].
	
	newParagraph focused: (self currentHand keyboardFocus == self).
	
	paragraph := newParagraph.
	self fit.

	^ paragraph
]

{ #category : #accessing }
AthensTextMorph >> paragraph [
	^ paragraph ifNil: [ paragraph := self createParagraph ].

]

{ #category : #accessing }
AthensTextMorph >> selectionColor: aColor [
	selectionColor := aColor.
	
]
