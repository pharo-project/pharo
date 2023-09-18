"
I am a font for special purpose like password or fallback.
I can show same form whenever someone requests any character.
"
Class {
	#name : #FixedFaceFont,
	#superclass : #AbstractFont,
	#instVars : [
		'baseFont',
		'substitutionCharacter'
	],
	#category : #'Graphics-Fonts'
}

{ #category : #accessing }
FixedFaceFont >> ascent [
	^baseFont ascent
]

{ #category : #accessing }
FixedFaceFont >> baseFont [
	^baseFont
]

{ #category : #accessing }
FixedFaceFont >> baseFont: aFont [
	baseFont := aFont
]

{ #category : #accessing }
FixedFaceFont >> baseKern [
	^baseFont baseKern
]

{ #category : #accessing }
FixedFaceFont >> characterFormAt: character [
	^ baseFont characterFormAt: substitutionCharacter
]

{ #category : #accessing }
FixedFaceFont >> descent [
	^baseFont descent
]

{ #category : #accessing }
FixedFaceFont >> descentKern [
	^baseFont descentKern
]

{ #category : #displaying }
FixedFaceFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY scale: scale [
	| size |
	size := stopIndex - startIndex + 1.
	^ baseFont
		displayString: (String new: size withAll: substitutionCharacter)
		on: aDisplayContext
		from: 1
		to: size
		at: aPoint
		kern: kernDelta
		baselineY: baselineY
		scale: scale
]

{ #category : #accessing }
FixedFaceFont >> emphasized: emph [
	^self class new baseFont: (baseFont emphasized: emph)
]

{ #category : #initialization }
FixedFaceFont >> errorFont [
	substitutionCharacter := $?
]

{ #category : #accessing }
FixedFaceFont >> familyName [
	^baseFont familyName, '-pw'
]

{ #category : #accessing }
FixedFaceFont >> fontSize: aNumber [
	self baseFont: (StrikeFont familyName: baseFont familyName size: aNumber) copy
]

{ #category : #private }
FixedFaceFont >> glyphInfoOf: aCharacter into: glyphInfoArray [

	^ baseFont glyphInfoOf: substitutionCharacter into: glyphInfoArray
]

{ #category : #accessing }
FixedFaceFont >> height [
	^baseFont height
]

{ #category : #initialization }
FixedFaceFont >> initialize [
	super initialize.
	baseFont := TextStyle defaultFont.
	self passwordFont
]

{ #category : #displaying }
FixedFaceFont >> installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale [
	^baseFont installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor scale: scale
]

{ #category : #accessing }
FixedFaceFont >> lineGrid [
	^baseFont lineGrid
]

{ #category : #accessing }
FixedFaceFont >> maxAscii [
	^ SmallInteger maxVal
]

{ #category : #accessing }
FixedFaceFont >> passwordCharacter [
	^$*
]

{ #category : #initialization }
FixedFaceFont >> passwordFont [
	substitutionCharacter := $*
]

{ #category : #accessing }
FixedFaceFont >> pointSize [
	^baseFont pointSize
]

{ #category : #caching }
FixedFaceFont >> releaseCachedState [
	baseFont releaseCachedState
]

{ #category : #measuring }
FixedFaceFont >> widthOf: aCharacter [
	^ baseFont widthOf: substitutionCharacter
]
