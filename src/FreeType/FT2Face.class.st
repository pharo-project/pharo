"
Do not rearrange these fields!
New fields should go at the end, because the plugin has to know about these indexes.

ByteArray representing a pointer to the malloc'd FT_Face struct:
handle

Copied from the FT_Face struct on creation:
numFaces faceIndex faceFlags styleFlags numGlyphs familyName styleName numFixedSizes availableSizes numCharmaps charmaps

Copied on creation, but only relevant to scalable outlines:
bbox unitsPerEm ascender descender height maxAdvanceWidth maxAdvanceHeight underlinePosition underlineThickness 

Working memory:
glyph -- FT2GlyphSlot, set by loadGlyph or loadChar
size -- the active size, set by activateSize, used by loadGlyph, getKerning, etc.
charmap -- set by setCharmap

"
Class {
	#name : #FT2Face,
	#superclass : #FT2Handle,
	#instVars : [
		'numFaces',
		'faceIndex',
		'faceFlags',
		'styleFlags',
		'numGlyphs',
		'familyName',
		'styleName',
		'numFixedSizes',
		'availableSizes',
		'numCharmaps',
		'charmaps',
		'bbox',
		'unitsPerEm',
		'ascender',
		'descender',
		'height',
		'maxAdvanceWidth',
		'maxAdvanceHeight',
		'underlinePosition',
		'underlineThickness',
		'glyph',
		'encoding',
		'platformId',
		'encodingId',
		'size'
	],
	#category : #FreeType-Base
}

{ #category : #rendering }
FT2Face >> angle: angle scale: scale offset: aPoint [
	| one matrix delta |
	one := (16r10000 * scale) asInteger.
	matrix := IntegerArray new: 4.
	angle isZero ifTrue: [
		matrix at: 1 put: one.
		matrix at: 4 put: one.
	] ifFalse: [
		| phi cos sin |
		phi := angle degreesToRadians.
		cos := (phi sin * one) rounded.
		sin := (phi cos * one) rounded.
		matrix at: 1 put: sin.
		matrix at: 2 put: cos negated.
		matrix at: 3 put: cos.
		matrix at: 4 put: sin.
 	].
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primSetTransform: matrix delta: delta.

]

{ #category : #rendering }
FT2Face >> angle: angle scale: scale offset: aPoint slant: slant [
	| one matrix delta slantOne |
	one := (16r10000 * scale) asInteger.
	slantOne := (16r10000 * scale* slant) asInteger.
	matrix := IntegerArray new: 4.
	angle isZero ifTrue: [
		matrix at: 1 put: one.
		matrix at: 2 put: slantOne.
		matrix at: 4 put: one.
	] ifFalse: [
		| phi cos sin |
		phi := angle degreesToRadians.
		cos := (phi sin * one) rounded.
		sin := (phi cos * one) rounded.
		matrix at: 1 put: sin.
		matrix at: 2 put: cos negated.
		matrix at: 3 put: cos.
		matrix at: 4 put: sin.
 	].
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primSetTransform: matrix delta: delta.

]

{ #category : #rendering }
FT2Face >> angle: angle scalePoint: scalePoint offset: aPoint [
	| oneX oneY matrix delta |
	oneX := (16r10000 * scalePoint x) asInteger.
	oneY :=  (16r10000 * scalePoint y) asInteger.
	matrix := IntegerArray new: 4.
	angle isZero ifTrue: [
		matrix at: 1 put: oneX.
		matrix at: 4 put: oneY.
	] ifFalse: [
		| phi cos sin |
		phi := angle degreesToRadians.
		cos := (phi sin * oneX) rounded.
		sin := (phi cos * oneY) rounded.
		matrix at: 1 put: sin.
		matrix at: 2 put: cos negated.
		matrix at: 3 put: cos.
		matrix at: 4 put: sin.
 	].
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primSetTransform: matrix delta: delta.

]

{ #category : #rendering }
FT2Face >> angle: angle scalePoint: scalePoint offset: aPoint slant: slant [
	| oneX oneY matrix delta slantOne|
	oneX := (16r10000 * scalePoint x) asInteger.
	oneY :=  (16r10000 * scalePoint y) asInteger.
	slantOne := (16r10000 * scalePoint x * slant) asInteger.
	matrix := IntegerArray new: 4.
	angle isZero ifTrue: [
		matrix at: 1 put: oneX.
		matrix at: 2 put: slantOne.
		matrix at: 4 put: oneY.
	] ifFalse: [
		| phi cos sin |
		phi := angle degreesToRadians.
		cos := (phi sin * oneX) rounded.
		sin := (phi cos * oneY) rounded.
		matrix at: 1 put: sin.
		matrix at: 2 put: cos negated.
		matrix at: 3 put: cos.
		matrix at: 4 put: sin.
 	].
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primSetTransform: matrix delta: delta.

]

{ #category : #accessing }
FT2Face >> ascender [
	^ascender
]

{ #category : #accessing }
FT2Face >> availableSizes [
	^availableSizes
]

{ #category : #accessing }
FT2Face >> bbox [
	bbox
		ifNil: [bbox := Rectangle new.
			self primLoadBbox: bbox].
	^ bbox
]

{ #category : #outlines }
FT2Face >> characterOutline: aCharacter [
	^self loadCharacterOutline: aCharacter asUnicode flags: LoadIgnoreTransform
]

{ #category : #accessing }
FT2Face >> charmaps [
	"Answer an Array of Strings naming the different character maps available for setCharMap:"
	charmaps ifNil: [
		charmaps := Array new: numCharmaps.
		self getCharMapsInto: charmaps ].
	^charmaps
]

{ #category : #accessing }
FT2Face >> descender [
	^descender
]

{ #category : #rendering }
FT2Face >> emboldenOutline: strength [
	^self primEmboldenGlyphSlotOutline: (strength * 64) rounded
]

{ #category : #accessing }
FT2Face >> encoding [
	encoding ifNil: [ self getCharMap ].
	^encoding
]

{ #category : #accessing }
FT2Face >> faceFlags [
	^faceFlags
]

{ #category : #accessing }
FT2Face >> faceIndex [
	^faceIndex
]

{ #category : #accessing }
FT2Face >> familyName [
	^familyName
]

{ #category : #charmaps }
FT2Face >> getCharMap [
	self primGetCharMap.
]

{ #category : #charmaps }
FT2Face >> getCharMapsInto: array [
	self primGetCharMapsInto: array.
]

{ #category : #accessing }
FT2Face >> glyph [
	glyph ifNil: [ glyph := FT2GlyphSlot fromFace: self ].
	^glyph
]

{ #category : #glyphs }
FT2Face >> glyphOfCharacter: aCharacter [
	"load a glyph with outline, glyph is not scaled "
	| em aGlyph |
	em := self unitsPerEm.
	self validate.
	self setPixelWidth: em height: em.
	self loadCharacter: aCharacter asInteger flags: LoadIgnoreTransform. "load glyph metrics"
	aGlyph := self glyph shallowCopy. " copy because 'face glyph' is only a slot"
	aGlyph outline: (self characterOutline: aCharacter).
	^aGlyph
]

{ #category : #glyphs }
FT2Face >> glyphOfCharacter: aCharacter fontSize: fontSize [
	^self glyphOfCharacter: aCharacter pixelSize: fontSize@fontSize
]

{ #category : #glyphs }
FT2Face >> glyphOfCharacter: aCharacter pixelSize: pixelSize [
	"load a glyph with outline, glyph is not scaled "
	| aGlyph |
	self validate.
	self setPixelWidth: pixelSize x height: pixelSize y.
	self loadCharacter: aCharacter asUnicode flags: LoadIgnoreTransform. "load glyph metrics"
	aGlyph := self glyph. " copy because 'face glyph' is only a slot"
	glyph := nil.
	^aGlyph
]

{ #category : #accessing }
FT2Face >> height [
	^height
]

{ #category : #testing }
FT2Face >> isBold [
	styleFlags == nil ifTrue:[^false].
	^styleFlags allMask: StyleFlagBold
]

{ #category : #testing }
FT2Face >> isFixedWidth [
	styleFlags == nil ifTrue:[^false].
	^faceFlags allMask: 4 "FT_FACE_FLAG_FIXED_WIDTH" 
]

{ #category : #testing }
FT2Face >> isItalic [
	styleFlags == nil ifTrue:[^false].
	^styleFlags allMask: StyleFlagItalic
]

{ #category : #testing }
FT2Face >> isRegular [
	styleFlags == nil ifTrue:[^true].
	^styleFlags = 0
]

{ #category : #kerning }
FT2Face >> kerningLeft: leftCharacter right: rightCharacter [
	[^self primGetKerningLeft: (self primGetCharIndex: leftCharacter asInteger)  
		right: (self primGetCharIndex: rightCharacter asInteger) 
	] on: FT2Error do:[:e | ^0@0]
]

{ #category : #glyphs }
FT2Face >> loadCharacter: index flags: flags [
	self primLoadCharacter: index flags: flags.
	glyph
		ifNil: [ glyph := FT2GlyphSlot fromFace: self ]
		ifNotNil: [ glyph loadFrom: self ].

]

{ #category : #outlines }
FT2Face >> loadCharacterOutline: index flags: flags [
	| em outline |
	em := unitsPerEm.
	self setPixelWidth: em height: em.
	self loadCharacter: index flags: flags.
	outline := FT2Outline new.
	outline primLoadSizesFrom: self.
	outline allocateArrays.
	outline primLoadArraysFrom: self.

	^outline
]

{ #category : #private }
FT2Face >> loadFields [
	self isValid 
		ifFalse: [ ^ self error: 'I cannot load face fields because face is not valid.' ].
	self primLoadFields.
]

{ #category : #glyphs }
FT2Face >> loadGlyph: index flags: flags [
	self primLoadGlyph: index flags: flags.
	glyph
		ifNil: [ glyph := FT2GlyphSlot fromFace: self ]
		ifNotNil: [ glyph loadFrom: self ].

]

{ #category : #accessing }
FT2Face >> maxAdvanceHeight [
	^maxAdvanceHeight
]

{ #category : #accessing }
FT2Face >> maxAdvanceWidth [
	^maxAdvanceWidth
]

{ #category : #accessing }
FT2Face >> memoryFaceData [
	self subclassResponsibility
]

{ #category : #initialize-release }
FT2Face >> newFaceFromExternalMemory: aFreeTypeExternalMemory index: anInteger [
	| memSize |

	aFreeTypeExternalMemory validate.
	memSize := aFreeTypeExternalMemory bytes size.
	self 
		primNewFaceFromExternalMemory: aFreeTypeExternalMemory 
		size: memSize 
		index: anInteger.
	
	self isValid
		ifTrue:[ self class register: self ]
]

{ #category : #initialize-release }
FT2Face >> newFaceFromFile: fileName index: anInteger [
	self primNewFaceFromFile: fileName fullName index: anInteger.
	self class register: self.
]

{ #category : #accessing }
FT2Face >> numCharmaps [
	^numCharmaps
]

{ #category : #accessing }
FT2Face >> numFaces [
	^numFaces
]

{ #category : #accessing }
FT2Face >> numFixedSizes [
	^numFixedSizes
]

{ #category : #accessing }
FT2Face >> numGlyphs [
	^numGlyphs
]

{ #category : #accessing }
FT2Face >> postscriptName [
	^self primGetPostscriptName
]

{ #category : #private-primitives }
FT2Face >> primDestroyHandle [
	<primitive: 'primitiveDoneFacePreserveFields' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primEmboldenGlyphSlotOutline: strengthInteger [
	<primitive: 'primitiveEmboldenFaceGlyphSlotOutline' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primGetCharIndex: characterCode [
	"Return the glyph index of a given character code"
	<primitive: 'primitiveGetFaceCharIndex' module: 'FT2Plugin'>
	^self primitiveFailed.

]

{ #category : #private-primitives }
FT2Face >> primGetCharMap [
	<primitive: 'primitiveGetFaceCharMap' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primGetCharMapsInto: array [
	<primitive: 'primitiveGetFaceCharMapsIntoArray' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primGetKerningLeft: leftGlyphIndex right: rightGlyphIndex [
	"self primGetKerningLeft: $V asInteger  right: $a asInteger "
	<primitive: 'primitiveGetKerningLeftRight' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primGetPostscriptName [ 
	<primitive: 'primitiveGetPostscriptName' module: 'FT2Plugin'>
	^nil
]

{ #category : #private-primitives }
FT2Face >> primGetTrackKerningPointSize: pointSize degree: degree [
	<primitive: 'primitiveGetTrackKerningPointSizeDegree' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primHasKerning [
	<primitive: 'primitiveHasKerning' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primLoadBbox: aRectangle [
	<primitive: 'primitiveLoadFaceBbox' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primLoadCharacter: index flags: flags [
	<primitive: 'primitiveLoadCharacter' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private }
FT2Face >> primLoadFields [
	<primitive: 'primitiveLoadFaceFields' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primLoadGlyph: index flags: flags [
	<primitive: 'primitiveLoadGlyph' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primNewFaceFromExternalMemory: aFreeTypeExternalMemory size: anInteger index: anInteger2 [
	<primitive: 'primitiveNewMemoryFaceFromExternalMemoryAndIndex' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primNewFaceFromFile: fileName index: anInteger [
	<primitive: 'primitiveNewFaceFromFileAndIndex' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primNewMemoryFaceByteSize: anInteger index: anInteger2 [
	<primitive: 'primitiveNewMemoryFaceByteSizeAndIndex' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primRenderGlyphIntoForm: aForm [
	<primitive: 'primitiveRenderGlyphIntoForm' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primRenderGlyphIntoForm: aForm pixelMode: anInteger [
	<primitive: 'primitiveRenderGlyphIntoFormWithRenderMode' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primSetBitmapLeft: x top: y [
	<primitive: 'primitiveSetBitmapLefttop' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primSetCharMap: encodingString [
	<primitive: 'primitiveSetFaceCharMap' module: 'FT2Plugin'>
	^self primitiveFailed
]

{ #category : #private-primitives }
FT2Face >> primSetPixelWidth: x height: y [
	<primitive: 'primitiveSetPixelSizes' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primSetTransform: matrixWordArray delta: deltaWordArray [
	"matrix is 16.16 fixed point
		x' = x*m[0] + y*m[1]                                 
		y' = x*m[2] + y*yy[3]
	delta is 26.6 fixed point
		x' = x + d[0]
		y' = y + d[1]
	"
	<primitive: 'primitiveSetTransform' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primTransformGlyphSlotOutline:  anIntegerArray [
	<primitive: 'primitiveTransformFaceGlyphSlotOutline' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #private-primitives }
FT2Face >> primTranslateGlyphSlotOutline:  anIntegerArray [
	<primitive: 'primitiveTranslateFaceGlyphSlotOutline' module: 'FT2Plugin'>
	^self primitiveFailed.
]

{ #category : #printing }
FT2Face >> printOn: aStream [ 
	super printOn: aStream.
	handle ifNil: [^self].
	"self familyName isNil ifTrue: [ self loadFields ]."
	aStream
		nextPut: $[;
		nextPutAll: (self familyName ifNil: ['?']);
		space;
		nextPutAll: (self styleName ifNil: ['?']);
		nextPut: $]
]

{ #category : #rendering }
FT2Face >> renderGlyphIntoForm: aForm [
	"render the current glyph (selected by loadChar/loadGlyph into the given form (1 or 8 bpp)"
	self primRenderGlyphIntoForm: aForm

]

{ #category : #rendering }
FT2Face >> renderGlyphIntoForm: aForm pixelMode: anInteger [
	"render the current glyph (selected by loadChar/loadGlyph into the given form (1 or 8 bpp)
	with pixel mode anInteger "
	self primRenderGlyphIntoForm: aForm pixelMode: anInteger

]

{ #category : #charmaps }
FT2Face >> setCharMap: encodingString [
	self primSetCharMap: encodingString.
	self primGetCharMap.

]

{ #category : #glyphs }
FT2Face >> setPixelWidth: x height: y [
	self primSetPixelWidth: x height: y
]

{ #category : #accessing }
FT2Face >> size [
	^size
]

{ #category : #accessing }
FT2Face >> styleFlags [
	^styleFlags
]

{ #category : #accessing }
FT2Face >> styleName [
	^styleName
]

{ #category : #rendering }
FT2Face >> transform: aMatrix [

	| matrix delta |
	matrix := IntegerArray new: 4.
	matrix at: 1 put: (aMatrix a11 * 16r10000) rounded.
	matrix at: 2 put: (aMatrix a12 * 16r10000) rounded.
	matrix at: 3 put: (aMatrix a21 * 16r10000) rounded.
	matrix at: 4 put: (aMatrix a22 * 16r10000) rounded.

	delta := IntegerArray new: 2.
	delta at: 1 put: (aMatrix a13 * 64) rounded. 
	delta at: 2 put: (aMatrix a23 * 64) rounded.
	self primSetTransform: matrix delta: delta.

]

{ #category : #rendering }
FT2Face >> transformOutlineAngle: angle scalePoint: scalePoint slant: slant [
	| oneX oneY matrix  slantOne|
	oneX := (16r10000 * scalePoint x) asInteger.
	oneY :=  (16r10000 * scalePoint y) asInteger.
	slantOne := (16r10000 * scalePoint x * slant) asInteger.
	matrix := IntegerArray new: 4.
	angle isZero ifTrue: [
		matrix at: 1 put: oneX.
		matrix at: 2 put: slantOne.
		matrix at: 4 put: oneY.
	] ifFalse: [
		| phi cos sin |
		phi := angle degreesToRadians.
		cos := (phi sin * oneX) rounded.
		sin := (phi cos * oneY) rounded.
		matrix at: 1 put: sin.
		matrix at: 2 put: cos negated.
		matrix at: 3 put: cos.
		matrix at: 4 put: sin.
 	].
	self primTransformGlyphSlotOutline: matrix
]

{ #category : #rendering }
FT2Face >> translateOutlineBy: aPoint [
	| delta|
	delta := IntegerArray new: 2.
	delta at: 1 put: (aPoint x * 64) rounded. 
	delta at: 2 put: (aPoint y * 64) rounded.
	self primTranslateGlyphSlotOutline: delta.
]

{ #category : #accessing }
FT2Face >> underlinePosition [
	^underlinePosition
]

{ #category : #accessing }
FT2Face >> underlineThickness [
	^underlineThickness
]

{ #category : #accessing }
FT2Face >> unitsPerEm [
	^unitsPerEm
]
