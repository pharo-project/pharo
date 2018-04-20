"
I implement the FT_Face_Rec, I am the main structure used by the library.
"
Class {
	#name : #FTFaceRec,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_ASCENDER',
		'OFFSET_AUTOHINT',
		'OFFSET_AVAILABLE_SIZES',
		'OFFSET_BBOX',
		'OFFSET_CHARMAP',
		'OFFSET_CHARMAPS',
		'OFFSET_DESCENDER',
		'OFFSET_DRIVER',
		'OFFSET_EXTENSIONS',
		'OFFSET_FACE_FLAGS',
		'OFFSET_FACE_INDEX',
		'OFFSET_FAMILY_NAME',
		'OFFSET_GENERIC',
		'OFFSET_GLYPH',
		'OFFSET_HEIGHT',
		'OFFSET_INTERNAL',
		'OFFSET_MAX_ADVANCE_HEIGHT',
		'OFFSET_MAX_ADVANCE_WIDTH',
		'OFFSET_MEMORY',
		'OFFSET_NUM_CHARMAPS',
		'OFFSET_NUM_FACES',
		'OFFSET_NUM_FIXED_SIZES',
		'OFFSET_NUM_GLYPHS',
		'OFFSET_SIZE',
		'OFFSET_SIZES_LIST',
		'OFFSET_STREAM',
		'OFFSET_STYLE_FLAGS',
		'OFFSET_STYLE_NAME',
		'OFFSET_UNDERLINE_POSITION',
		'OFFSET_UNDERLINE_THICKNESS',
		'OFFSET_UNITS_PER_EM'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTFaceRec class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
    FT_Long           num_faces;
    FT_Long           face_index;

    FT_Long           face_flags;
    FT_Long           style_flags;

    FT_Long           num_glyphs;

    FT_String*        family_name;
    FT_String*        style_name;

    FT_Int            num_fixed_sizes;
    FT_Bitmap_Size*   available_sizes;

    FT_Int            num_charmaps;
    void*       		 charmaps;

    FT_Generic        generic;

    FT_BBox           bbox;

    FT_UShort         units_per_EM;
    FT_Short          ascender;
    FT_Short          descender;
    FT_Short          height;

    FT_Short          max_advance_width;
    FT_Short          max_advance_height;

    FT_Short          underline_position;
    FT_Short          underline_thickness;

    FT_GlyphSlot      glyph;
    FT_Size           size;
    FT_CharMap        charmap;

    FT_Driver         driver;
    FT_Memory         memory;
    FT_Stream         stream;

    FT_ListRec        sizes_list;

    FT_Generic        autohint;   
    void*             extensions; 

    FT_Face_Internal  internal;
	)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> ascender [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_ASCENDER
]

{ #category : #'accessing structure variables' }
FTFaceRec >> ascender: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_ASCENDER put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> autohint [
	"This method was automatically generated"
	^ FTGeneric fromHandle: (handle referenceStructAt: OFFSET_AUTOHINT length: FTGeneric byteSize)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> autohint: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_AUTOHINT put: anObject getHandle length: FTGeneric byteSize
]

{ #category : #'accessing structure variables' }
FTFaceRec >> available_sizes [
	"This method was automatically generated"
	^FTBitmapSize fromHandle: (handle pointerAt: OFFSET_AVAILABLE_SIZES)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> available_sizes: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_AVAILABLE_SIZES put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> bbox [
	"This method was automatically generated"
	^ FTBBox fromHandle: (handle referenceStructAt: OFFSET_BBOX length: FTBBox byteSize)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> bbox: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_BBOX put: anObject getHandle length: FTBBox byteSize
]

{ #category : #'accessing structure variables' }
FTFaceRec >> charmap [
	"This method was automatically generated"
	^FTCharMapRec fromHandle: (handle pointerAt: OFFSET_CHARMAP)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> charmap: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_CHARMAP put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> charmaps [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_CHARMAPS) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> charmaps: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_CHARMAPS put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> descender [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_DESCENDER
]

{ #category : #'accessing structure variables' }
FTFaceRec >> descender: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_DESCENDER put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> driver [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_DRIVER) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> driver: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_DRIVER put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> extensions [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_EXTENSIONS) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> extensions: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_EXTENSIONS put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> face_flags [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_FACE_FLAGS
]

{ #category : #'accessing structure variables' }
FTFaceRec >> face_flags: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_FACE_FLAGS put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> face_index [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_FACE_INDEX
]

{ #category : #'accessing structure variables' }
FTFaceRec >> face_index: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_FACE_INDEX put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> family_name [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_FAMILY_NAME) type: ExternalType char asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> family_name: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_FAMILY_NAME put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> generic [
	"This method was automatically generated"
	^ FTGeneric fromHandle: (handle referenceStructAt: OFFSET_GENERIC length: FTGeneric byteSize)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> generic: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_GENERIC put: anObject getHandle length: FTGeneric byteSize
]

{ #category : #'accessing structure variables' }
FTFaceRec >> glyph [
	"This method was automatically generated"
	^FTGlyphSlotRec fromHandle: (handle pointerAt: OFFSET_GLYPH)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> glyph: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_GLYPH put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> height [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_HEIGHT
]

{ #category : #'accessing structure variables' }
FTFaceRec >> height: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_HEIGHT put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> internal [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_INTERNAL) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> internal: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_INTERNAL put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> max_advance_height [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_MAX_ADVANCE_HEIGHT
]

{ #category : #'accessing structure variables' }
FTFaceRec >> max_advance_height: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_MAX_ADVANCE_HEIGHT put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> max_advance_width [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_MAX_ADVANCE_WIDTH
]

{ #category : #'accessing structure variables' }
FTFaceRec >> max_advance_width: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_MAX_ADVANCE_WIDTH put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> memory [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_MEMORY) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> memory: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_MEMORY put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_charmaps [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_NUM_CHARMAPS
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_charmaps: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_NUM_CHARMAPS put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_faces [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_NUM_FACES
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_faces: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_NUM_FACES put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_fixed_sizes [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_NUM_FIXED_SIZES
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_fixed_sizes: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_NUM_FIXED_SIZES put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_glyphs [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_NUM_GLYPHS
]

{ #category : #'accessing structure variables' }
FTFaceRec >> num_glyphs: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_NUM_GLYPHS put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> size [
	"This method was automatically generated"
	^FTSizeRec fromHandle: (handle pointerAt: OFFSET_SIZE)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> size: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_SIZE put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> sizes_list [
	"This method was automatically generated"
	^ FTListRec fromHandle: (handle referenceStructAt: OFFSET_SIZES_LIST length: FTListRec byteSize)
]

{ #category : #'accessing structure variables' }
FTFaceRec >> sizes_list: anObject [
	"This method was automatically generated"
	handle structAt: OFFSET_SIZES_LIST put: anObject getHandle length: FTListRec byteSize
]

{ #category : #'accessing structure variables' }
FTFaceRec >> stream [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_STREAM) type: ExternalType void asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> stream: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_STREAM put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> style_flags [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_STYLE_FLAGS
]

{ #category : #'accessing structure variables' }
FTFaceRec >> style_flags: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_STYLE_FLAGS put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> style_name [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_STYLE_NAME) type: ExternalType char asPointerType
]

{ #category : #'accessing structure variables' }
FTFaceRec >> style_name: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_STYLE_NAME put: anObject getHandle.
]

{ #category : #'accessing structure variables' }
FTFaceRec >> underline_position [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_UNDERLINE_POSITION
]

{ #category : #'accessing structure variables' }
FTFaceRec >> underline_position: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_UNDERLINE_POSITION put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> underline_thickness [
	"This method was automatically generated"
	^handle signedShortAt: OFFSET_UNDERLINE_THICKNESS
]

{ #category : #'accessing structure variables' }
FTFaceRec >> underline_thickness: anObject [
	"This method was automatically generated"
	handle signedShortAt: OFFSET_UNDERLINE_THICKNESS put: anObject
]

{ #category : #'accessing structure variables' }
FTFaceRec >> units_per_EM [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_UNITS_PER_EM
]

{ #category : #'accessing structure variables' }
FTFaceRec >> units_per_EM: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_UNITS_PER_EM put: anObject
]
