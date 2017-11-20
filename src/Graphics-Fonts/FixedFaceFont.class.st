"
I am a font for special purpose like password or fallback.
I can show same form whenever someone requests any character.

Variable displaySelector is future use to show a form dynamically.
(Although it would be unnecessary...)
"
Class {
	#name : #FixedFaceFont,
	#superclass : #AbstractFont,
	#instVars : [
		'baseFont',
		'substitutionCharacter',
		'displaySelector'
	],
	#category : #Graphics-Fonts
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
FixedFaceFont >> displayErrorOn: aCanvas length: length at: aPoint kern: kernDelta [ 
	| maskedString |
	maskedString := String new: length.
	maskedString atAllPut: substitutionCharacter.
	^ baseFont
		displayString: maskedString
		on: aCanvas
		from: 1
		to: length
		at: aPoint
		kern: kernDelta
]

{ #category : #displaying }
FixedFaceFont >> displayErrorOn: aCanvas length: length at: aPoint kern: kernDelta baselineY: baselineY [
	| maskedString |
	maskedString := String new: length.
	maskedString atAllPut: substitutionCharacter.
	^ baseFont
		displayString: maskedString
		on: aCanvas
		from: 1
		to: length
		at: aPoint
		kern: kernDelta
		baselineY: baselineY
]

{ #category : #displaying }
FixedFaceFont >> displayPasswordOn: aCanvas length: length at: aPoint kern: kernDelta [ 
	| maskedString |
	maskedString := String new: length.
	maskedString atAllPut: substitutionCharacter.
	^ baseFont
		displayString: maskedString
		on: aCanvas
		from: 1
		to: length
		at: aPoint
		kern: kernDelta
]

{ #category : #displaying }
FixedFaceFont >> displayPasswordOn: aCanvas length: length at: aPoint kern: kernDelta baselineY: baselineY [
	| maskedString |
	maskedString := String new: length.
	maskedString atAllPut: substitutionCharacter.
	^ baseFont
		displayString: maskedString
		on: aCanvas
		from: 1
		to: length
		at: aPoint
		kern: kernDelta
		baselineY: baselineY
]

{ #category : #displaying }
FixedFaceFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta [ 
	| size |
	size := stopIndex - startIndex + 1.
	^ self perform: displaySelector withArguments: (Array with: aDisplayContext with: size with: aPoint with: kernDelta with: aPoint y + self ascent).
]

{ #category : #displaying }
FixedFaceFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [
	| size |
	size := stopIndex - startIndex + 1.
	^ self perform: displaySelector withArguments: (Array with: aDisplayContext with: size with: aPoint with: kernDelta with: baselineY).
]

{ #category : #displaying }
FixedFaceFont >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta from: fromFont [ 
	| destPoint |
	destPoint := self
				displayString: aString
				on: aBitBlt
				from: startIndex
				to: stopIndex
				at: aPoint
				kern: kernDelta.
	^ Array with: stopIndex + 1 with: destPoint
]

{ #category : #displaying }
FixedFaceFont >> displayString: aString on: aBitBlt from: startIndex to: stopIndex at: aPoint kern: kernDelta from: fromFont baselineY: baselineY [
	| destPoint |
	destPoint := self
				displayString: aString
				on: aBitBlt
				from: startIndex
				to: stopIndex
				at: aPoint
				kern: kernDelta
				baselineY: baselineY.
	^ Array with: stopIndex + 1 with: destPoint
]

{ #category : #accessing }
FixedFaceFont >> emphasized: emph [
	^self class new baseFont: (baseFont emphasized: emph)
]

{ #category : #initialization }
FixedFaceFont >> errorFont [
	displaySelector := #displayErrorOn:length:at:kern:baselineY:.
	substitutionCharacter := $?.
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

	^ baseFont glyphInfoOf: substitutionCharacter into: glyphInfoArray.

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
FixedFaceFont >> installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor [
	^baseFont installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor
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
	displaySelector := #displayPasswordOn:length:at:kern:baselineY:.
	substitutionCharacter := $*
]

{ #category : #accessing }
FixedFaceFont >> pointSize [
	^baseFont pointSize
]

{ #category : #caching }
FixedFaceFont >> releaseCachedState [
	baseFont releaseCachedState.
]

{ #category : #measuring }
FixedFaceFont >> widthOf: aCharacter [ 
	^ baseFont widthOf: substitutionCharacter
]
