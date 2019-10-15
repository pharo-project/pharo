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
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Base'
}

{ #category : #finalization }
FT2Face class >> finalizeResourceData: handle [

	handle ifNil: [ ^ self ].
	handle isNull
		ifTrue: [ ^ self ].

	FT2Library current ffiDoneFace: handle.
	handle beNull
]

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
	| returnCode outline faceRec |

	self validate.
	faceRec := FTFaceRec fromHandle: handle.
	outline := FTOutline fromHandle: faceRec glyph outline getHandle asExternalAddress.
	returnCode := self ffiOutline: outline embolden:  (strength * 64) rounded.

	returnCode ~= 0
		ifTrue: [ FT2Error errorCode: returnCode signal: 'Error embolding glyph' ]
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

{ #category : #'private-ffi-calls' }
FT2Face >> ffiGetCharIndex: charIndex [
	^ self ffiCall: #(FT_UInt FT_Get_Char_Index(self, FT_ULong charIndex))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiGetKerningLeft: left_glyph right: right_glyph result: akerning [
	self
		ffiCall: #(FT_Error FT_Get_Kerning(self, FT_UInt left_glyph, FT_UInt right_glyph, 2, FT_Vector  *akerning ))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiGetPostscriptName [
	self ffiCall: #(char* FT_Get_Postscript_Name(self))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiLoadChar: char_code flags: load_flags [
	^ self ffiCall: #(FT_Error FT_Load_Char(self, FT_ULong  char_code, FT_Int32  load_flags ))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiLoadGlyph: glyph_index flags: load_flags [
	^ self ffiCall: #(FT_Error FT_Load_Glyph(self, FT_UInt glyph_index,FT_Int32  load_flags ))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiOutline: outline embolden: strength [

	^ self ffiCall: #(FT_Error FT_Outline_Embolden(FT_Outline*  outline, FT_Pos strength ))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiOutline: outline transformByMatrix: matrix [
	^ self ffiCall: #(void FT_Outline_Transform( FT_Outline*  outline, FT_Matrix*   matrix ))

]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiSelectCharmap: encoding_value [
	self ffiCall: #(FT_Error FT_Select_Charmap(self, FT_Encoding  encoding_value ))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiSetPixelWidth: pixel_width height: pixel_height [
	^ self ffiCall: #(long FT_Set_Pixel_Sizes(self,
                      FT_UInt  pixel_width,
                      FT_UInt  pixel_height ))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiTransformMatrix: matrix delta: delta [
	self ffiCall: #(void FT_Set_Transform(self, FT_Matrix*  matrix, FT_Vector*  delta ))
]

{ #category : #'private-ffi-calls' }
FT2Face >> ffiTranslateOutline: outline x: xOffset y: yOffset [
	self ffiCall: #(void FT_Outline_Translate (FT_Outline * outline , FT_Pos xOffset , FT_Pos yOffset))
]

{ #category : #glyphs }
FT2Face >> fillGlyph: aGlyphSlot [
	| faceRec |
	
	self validate.
	
	aGlyphSlot face: self.	
	faceRec := FTFaceRec fromHandle: handle.
	aGlyphSlot fillFromGlyphSlotRec: faceRec glyph. 
]

{ #category : #charmaps }
FT2Face >> getCharMap [
	|faceRec|
	self isValid ifFalse: [ self error: 'The Face is not valid.' ].
	faceRec := FTFaceRec fromHandle: handle.
	
	faceRec charmap isNull ifTrue: [ ^ self ].
	encoding := faceRec charmap encoding asByteArray asString.
	platformId := faceRec charmap platform_id.
	encodingId := faceRec charmap encoding_id.
]

{ #category : #charmaps }
FT2Face >> getCharMapsInto: array [
	| faceRec charmap external |
	
	faceRec := FTFaceRec fromHandle: handle.

	external := FFIExternalArray fromHandle: faceRec charmaps getHandle type: 'void*' size: faceRec num_charmaps.
	
	1 to: faceRec num_charmaps do: [ :idx |
		charmap := FTCharMapRec fromHandle: (external at: idx).
		array at: idx put: charmap encoding asByteArray asString		
	].

	^ array.

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
	^ self
		primGetKerningLeft: (self ffiGetCharIndex: leftCharacter asInteger)
		right: (self ffiGetCharIndex: rightCharacter asInteger)
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
	| faceRect |
	self isValid 
		ifFalse: [ ^ self error: 'I cannot load face fields because face is not valid.' ].

	faceRect := FTFaceRec fromHandle: handle.
	
	numFaces := faceRect num_faces.	
	faceIndex := faceRect face_index.
	faceFlags := faceRect face_flags.
	styleFlags := faceRect style_flags.
	numGlyphs := faceRect num_glyphs.
	familyName := faceRect family_name readString.
	styleName := faceRect style_name readString. 
	
	numFixedSizes := faceRect num_fixed_sizes.
	numCharmaps := faceRect num_charmaps.
	charmaps := nil.
	encoding := nil.
	
	"FT_FACE_FLAG_SCALABLE = 1 "
	(faceRect face_flags bitAnd: 1) ~= 0 ifTrue: [  
		unitsPerEm := faceRect units_per_EM.
		ascender := faceRect ascender.
		descender := faceRect descender.
		height := faceRect height.
		maxAdvanceWidth := faceRect max_advance_width.
		maxAdvanceHeight := faceRect max_advance_height.
		underlinePosition := faceRect underline_position.
		underlineThickness := faceRect underline_thickness.
	].
	
	bbox := Rectangle 
		origin: ( Point x: faceRect bbox xMin y: faceRect bbox yMin )
		corner: ( Point x: faceRect bbox xMax y: faceRect bbox yMax ).

]

{ #category : #glyphs }
FT2Face >> loadGlyph: index flags: flags [
	| returnCode |
	
	returnCode := self ffiLoadGlyph: index flags: flags.
	returnCode ~= 0 ifTrue: [ FT2Error errorCode: returnCode signal: 'Loading glyph'  ].
	
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

{ #category : #initialization }
FT2Face >> newFaceFromExternalMemory: aFreeTypeExternalMemory index: anInteger [
	| memSize holder returnCode |

	aFreeTypeExternalMemory validate.
	memSize := aFreeTypeExternalMemory bytes size.
	holder := PointerHolder new.
	
	FT2Library current checkLibrary.
	returnCode := FT2Library current ffiNewFace: holder fromMemory: aFreeTypeExternalMemory handle size: memSize  index: anInteger.
	returnCode ~= 0
		ifTrue: [ FT2Error errorCode: returnCode signal: 'Error reading new face from memory'].
		
	handle := holder value.
	self autoRelease.
]

{ #category : #initialization }
FT2Face >> newFaceFromFile: file index: anInteger [
	| holder returnCode fileName |
	
	holder := PointerHolder new.
	fileName := file asFileReference pathString.
	
	FT2Library current checkLibrary.
	returnCode := FT2Library current ffiNewFace: holder fromFilename: fileName index: anInteger.
	returnCode ~= 0
		ifTrue: [ FT2Error errorCode: returnCode signal: 'Error reading new face from file'].
		
	handle := holder value.
	self autoRelease.
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
FT2Face >> outline [
	self validate.
	^ FTOutline fromHandle: (FTFaceRec fromHandle: handle) glyph outline getHandle asExternalAddress
]

{ #category : #accessing }
FT2Face >> postscriptName [
	self isValid ifFalse: [ ^ nil ].	
	^self ffiGetPostscriptName
]

{ #category : #'private-primitives' }
FT2Face >> primGetKerningLeft: leftGlyphIndex right: rightGlyphIndex [
	| result returnCode |
	result := FTVector new.

	returnCode := self
		ffiGetKerningLeft: leftGlyphIndex
		right: rightGlyphIndex
		result: result.

	returnCode ~= 0
		ifTrue: [ ^ 0 @ 0 ].
	
	^ result x @ result y
]

{ #category : #'private-primitives' }
FT2Face >> primLoadCharacter: index flags: flags [
	| returnCode |
	self validate.
	returnCode := self ffiLoadChar: index flags: flags.
	returnCode ~= 0 ifTrue: [ FT2Error errorCode: returnCode signal: 'Error loading character' ].
]

{ #category : #'private-primitives' }
FT2Face >> primSetTransform: matrixWordArray delta: deltaWordArray [
	"matrix is 16.16 fixed point
		x' = x*m[0] + y*m[1]                                 
		y' = x*m[2] + y*yy[3]
	delta is 26.6 fixed point
		x' = x + d[0]
		y' = y + d[1]
	"
	
	| matrix delta |
	matrix := FTMatrix new.
	delta := FTVector new.

	delta x: (deltaWordArray at: 1).
	delta y: (deltaWordArray at: 2).
	
	matrix xx: (matrixWordArray at: 1).
	matrix xy: (matrixWordArray at: 2).
	matrix yx: (matrixWordArray at: 3).
	matrix yy: (matrixWordArray at: 4).
		
	self ffiTransformMatrix: matrix delta: delta.
]

{ #category : #'private-primitives' }
FT2Face >> primTransformGlyphSlotOutline:  anIntegerArray [
	| faceRec outline matrix |
	faceRec := FTFaceRec fromHandle: handle.
	matrix := FTMatrix new.
	
	matrix xx: (anIntegerArray at:1). 
	matrix xy: (anIntegerArray at:2). 
	matrix yx: (anIntegerArray at:3). 
	matrix yy: (anIntegerArray at:4).  
	
	"I need to obtain the real address of the struct inside the struct"
	outline := FTOutline fromHandle: faceRec glyph outline getHandle asExternalAddress.
	
	self ffiOutline: outline transformByMatrix: matrix.
]

{ #category : #'private-primitives' }
FT2Face >> primTranslateGlyphSlotOutline:  anIntegerArray [
	self ffiTranslateOutline: self outline x: (anIntegerArray at: 1) y: (anIntegerArray at: 2).
	
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
	self validate.
	self renderGlyphIntoForm: aForm pixelMode: nil

]

{ #category : #rendering }
FT2Face >> renderGlyphIntoForm: aForm pixelMode: pixelMode [
	"render the current glyph (selected by loadChar/loadGlyph into the given form (1 or 8 bpp)
	with pixel mode anInteger "

	| ftBitmap buffer outline ftFace |

	ftBitmap := FTBitmap new.

	ftFace := FTFaceRec fromHandle: handle.
	outline := self outline.

	buffer := ExternalAddress allocate: aForm bits byteSize.
	1 to: aForm bits byteSize do: [ :i | buffer byteAt: i put: 0 ].
	buffer autoRelease.

	ftBitmap setBuffer: buffer.
	ftBitmap initializeFromForm: aForm pixelMode: pixelMode.

	FT2Library current getBitmap: ftBitmap getHandle fromOutline: outline getHandle.

	aForm bits copyFromByteArray: (buffer copyFrom: 1 to: aForm bits byteSize)
]

{ #category : #charmaps }
FT2Face >> setCharMap: encodingString [
	| returnCode |
	
	self validate.
	
	returnCode := self ffiSelectCharmap: encodingString asByteArray asInteger.
	returnCode ~= 0 ifTrue: [ FT2Error errorCode: returnCode signal: 'Error selecting charmap' ].	

	self getCharMap.

]

{ #category : #glyphs }
FT2Face >> setPixelWidth: x height: y [
	| returnCode |
	self isValid ifFalse: [ self error: 'The Face is not valid' ].
	returnCode := self ffiSetPixelWidth: x height: y.
	
	returnCode ~= 0 ifTrue: [ FT2Error errorCode: returnCode signal: 'Error setting the pixel width and height.' ].
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
