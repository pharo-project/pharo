"
see BorderedMorph
"
Class {
	#name : #InsetBorder,
	#superclass : #SimpleBorder,
	#category : #Morphic-Base-Borders
}

{ #category : #accessing }
InsetBorder >> bottomRightColor [
	"Changed from direct access to color since, if nil,
	self color is transparent."
	
	^width = 1 
		ifTrue: [self color twiceLighter]
		ifFalse: [self color lighter]
]

{ #category : #accessing }
InsetBorder >> colorsAtCorners [
	| c c14 c23 |
	c := self color.
	c14 := c lighter. c23 := c darker.
	^Array with: c23 with: c14 with: c14 with: c23.
]

{ #category : #accessing }
InsetBorder >> style [
	^#inset
]

{ #category : #accessing }
InsetBorder >> topLeftColor [
	"Changed from direct access to color since, if nil,
	self color is transparent."
	
	^width = 1 
		ifTrue: [self color twiceDarker]
		ifFalse: [self color darker]
]

{ #category : #'color tracking' }
InsetBorder >> trackColorFrom: aMorph [
	baseColor ifNil:[self color: aMorph insetColor].
]
