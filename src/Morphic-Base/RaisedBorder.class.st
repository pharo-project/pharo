"
see BorderedMorph
"
Class {
	#name : #RaisedBorder,
	#superclass : #SimpleBorder,
	#category : 'Morphic-Base-Borders'
}

{ #category : #accessing }
RaisedBorder >> bottomRightColor [
	"Changed from direct access to color since, if nil,
	self color is transparent."
	
	^width = 1 
		ifTrue: [self color twiceDarker]
		ifFalse: [self color darker]
]

{ #category : #accessing }
RaisedBorder >> colorsAtCorners [
	| c c14 c23 |
	c := self color.
	c14 := c lighter. c23 := c darker.
	^Array with: c14 with: c23 with: c23 with: c14
]

{ #category : #accessing }
RaisedBorder >> style [
	^#raised
]

{ #category : #accessing }
RaisedBorder >> topLeftColor [
	"Changed from direct access to color since, if nil,
	self color is transparent."

	^width = 1 
		ifTrue: [self color twiceLighter]
		ifFalse: [self color lighter]
]

{ #category : #'color tracking' }
RaisedBorder >> trackColorFrom: aMorph [
	baseColor ifNil:[self color: aMorph raisedColor].
]
