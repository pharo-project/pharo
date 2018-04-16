"
I Implement the FT_GlyphSlot_Rec, I am the structure where the glyphs are stored when reading from a font.
"
Class {
	#name : #FTGlyphSlotRec,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_ADVANCE',
		'OFFSET_BITMAP',
		'OFFSET_BITMAP_LEFT',
		'OFFSET_BITMAP_TOP',
		'OFFSET_CONTROL_DATA',
		'OFFSET_CONTROL_LEN',
		'OFFSET_FACE',
		'OFFSET_FORMAT',
		'OFFSET_GENERIC',
		'OFFSET_INTERNAL',
		'OFFSET_LIBRARY',
		'OFFSET_LINEARHORIADVANCE',
		'OFFSET_LINEARVERTADVANCE',
		'OFFSET_LSB_DELTA',
		'OFFSET_METRICS',
		'OFFSET_NEXT',
		'OFFSET_NUM_SUBGLYPHS',
		'OFFSET_OTHER',
		'OFFSET_OUTLINE',
		'OFFSET_RESERVED',
		'OFFSET_RSB_DELTA',
		'OFFSET_SUBGLYPHS'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTGlyphSlotRec class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
    FT_Library        library;
    FT_Face           face;
    FT_GlyphSlot      next;
    FT_UInt           reserved;
    FT_Generic        generic;

    FT_Glyph_Metrics  metrics;
    FT_Fixed          linearHoriAdvance;
    FT_Fixed          linearVertAdvance;
    FT_Vector         advance;

    FT_Glyph_Format   format;

    FT_Bitmap         bitmap;
    FT_Int            bitmap_left;
    FT_Int            bitmap_top;

    FT_Outline        outline;

    FT_UInt           num_subglyphs;
    FT_SubGlyph       subglyphs;

    void*             control_data;
    long              control_len;

    FT_Pos            lsb_delta;
    FT_Pos            rsb_delta;

    void*             other;

    FT_Slot_Internal  internal;)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> advance [
	"This method was automatically generated"
	^ FTVector fromHandle: (handle referenceStructAt: OFFSET_ADVANCE length: FTVector byteSize)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> advance: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_ADVANCE put: anObject getHandle length: FTVector byteSize
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> bitmap [
	"This method was automatically generated"
	^ FTBitmap fromHandle: (handle referenceStructAt: OFFSET_BITMAP length: FTBitmap byteSize)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> bitmap: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_BITMAP put: anObject getHandle length: FTBitmap byteSize
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> bitmap_left [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_BITMAP_LEFT
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> bitmap_left: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_BITMAP_LEFT put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> bitmap_top [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_BITMAP_TOP
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> bitmap_top: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_BITMAP_TOP put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> control_data [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_CONTROL_DATA) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> control_data: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_CONTROL_DATA put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> control_len [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_CONTROL_LEN
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> control_len: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_CONTROL_LEN put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> face [
	"This method was automatically generated"
	^FTFaceRec fromHandle: (handle pointerAt: OFFSET_FACE)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> face: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_FACE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> format [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_FORMAT
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> format: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_FORMAT put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> generic [
	"This method was automatically generated"
	^ FTGeneric fromHandle: (handle referenceStructAt: OFFSET_GENERIC length: FTGeneric byteSize)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> generic: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_GENERIC put: anObject getHandle length: FTGeneric byteSize
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> internal [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_INTERNAL) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> internal: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_INTERNAL put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> library [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_LIBRARY) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> library: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_LIBRARY put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> linearHoriAdvance [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LINEARHORIADVANCE
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> linearHoriAdvance: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LINEARHORIADVANCE put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> linearVertAdvance [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LINEARVERTADVANCE
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> linearVertAdvance: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LINEARVERTADVANCE put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> lsb_delta [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LSB_DELTA
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> lsb_delta: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_LSB_DELTA put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> metrics [
	"This method was automatically generated"
	^ FTGlyphMetrics fromHandle: (handle referenceStructAt: OFFSET_METRICS length: FTGlyphMetrics byteSize)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> metrics: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_METRICS put: anObject getHandle length: FTGlyphMetrics byteSize
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> next [
	"This method was automatically generated"
	^FTGlyphSlotRec fromHandle: (handle pointerAt: OFFSET_NEXT)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> next: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_NEXT put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> num_subglyphs [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_NUM_SUBGLYPHS
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> num_subglyphs: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_NUM_SUBGLYPHS put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> other [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_OTHER) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> other: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_OTHER put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> outline [
	"This method was automatically generated"
	^ FTOutline fromHandle: (handle referenceStructAt: OFFSET_OUTLINE length: FTOutline byteSize)
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> outline: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_OUTLINE put: anObject getHandle length: FTOutline byteSize
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> reserved [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_RESERVED
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> reserved: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_RESERVED put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> rsb_delta [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_RSB_DELTA
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> rsb_delta: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_RSB_DELTA put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> subglyphs [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_SUBGLYPHS) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTGlyphSlotRec >> subglyphs: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_SUBGLYPHS put: anObject getHandle.
]
