Extension { #name : #Morph }

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> asBrick [
	
	^ GLMMorphBrick on: self
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> brickApi [

	^ self valueOfProperty: #brickApi ifAbsentPut: [ GLMBrickMorphWrapper on: self ]
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> brickDoubleLink [

	^ self valueOfProperty: #brickDoubleLink ifAbsentPut: [ nil ]
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> brickDoubleLink: aDoubleLink [

	self setProperty: #brickDoubleLink toValue: aDoubleLink
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> brickImageForm: aBackground [
	| background form brickBorder |
	
	self isBrick ifFalse: [
		^ self imageForm ].

	brickBorder := self border.
	self border: GLMBrickBorder new.
	
	background := self color.
	self color: aBackground.
	form := self imageForm.
	self color: background.
	
	self border: brickBorder.
	
	^ form
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> brickThemer [
	^ (self nearestOwnerThat: [ :each | (each isKindOf: GLMFoundationBrick) | (each isKindOf: GLMBrick) ])
		ifNotNil: [ :brick | brick themer ]
		ifNil: [ self theme brickThemer ]
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> brickThemerChanged [

	self submorphs do: #brickThemerChanged
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> brickTransparentImageForm [
	|background form brickBorder|
	
	self isBrick ifTrue: [ 
		brickBorder := self border.
		self border: GLMBrickBorder new ].
	
	background := self color.
	self color: Color transparent.
	form := self imageForm.
	self color: background.
	
	self isBrick ifTrue: [ 
		self border: brickBorder ].
	^ form
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> exportAsPNGSilently [
	| fName time |
	time := (DateAndTime now asString copyReplaceAll: ':' with: '-') copyReplaceAll: '.' with: '-'.
	fName := self externalName, time , '.png'.
	PNGReadWriter putForm: self imageForm onFileNamed: fName
]

{ #category : #'*Glamour-Morphic-Brick' }
Morph >> isBrick [

	^ false
]
