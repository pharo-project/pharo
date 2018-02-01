"
Simple glyph infos
"
Class {
	#name : #SimpleTextGlyphInfo,
	#superclass : #FT2GlyphSlot,
	#category : #'Athens-Text'
}

{ #category : #'instance-creation' }
SimpleTextGlyphInfo class >> fromFace: anFT2Face [
	^(self new)
		loadFrom: anFT2Face;
		yourself.
]

{ #category : #accessing }
SimpleTextGlyphInfo >> face [
	^ face
]

{ #category : #initialization }
SimpleTextGlyphInfo >> loadFrom: anFT2Face [
	"Same as in superclass, but avoid converting coordinates to not waste CPU cycles"
	
	face := anFT2Face.
	self primLoadFrom: anFT2Face.
	format := (EndianDetector isLittleEndian ifTrue: [ format reversed ] ifFalse: [ format ]) asString.

]
