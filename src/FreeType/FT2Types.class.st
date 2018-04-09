"
I am a shared pool with all the mapping to the types used from the FreeType library.
"
Class {
	#name : #FT2Types,
	#superclass : #SharedPool,
	#classVars : [
		'FT_BBox',
		'FT_Bitmap',
		'FT_Bitmap_Size',
		'FT_CharMap',
		'FT_Driver',
		'FT_Encoding',
		'FT_Error',
		'FT_Face',
		'FT_Face_Internal',
		'FT_Fixed',
		'FT_Generic',
		'FT_GlyphSlot',
		'FT_Glyph_Format',
		'FT_Glyph_Metrics',
		'FT_Int',
		'FT_Int32',
		'FT_Library',
		'FT_ListNode',
		'FT_ListRec',
		'FT_Long',
		'FT_Matrix',
		'FT_Memory',
		'FT_Outline',
		'FT_Pos',
		'FT_Short',
		'FT_Size',
		'FT_Size_Internal',
		'FT_Size_Metrics',
		'FT_Slot_Internal',
		'FT_Stream',
		'FT_String',
		'FT_SubGlyph',
		'FT_UInt',
		'FT_ULong',
		'FT_UShort',
		'FT_Vector',
		'IntHolder',
		'PointerHolder'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'class initialization' }
FT2Types class >> initialize [
	"self initialize"
	IntHolder := FFIExternalValueHolder ofType: 'int'.
	PointerHolder := FFIExternalValueHolder ofType: 'void*'.
	
	FT_Error := 'long'.
	FT_Long := 'long'.
	FT_ULong := 'ulong'.
	FT_UShort := 'ushort'.
	FT_Short := 'short'.
	FT_UInt := 'uint'.
	FT_Int32 := 'int32'.
	
	FT_Pos := 'long'.
	FT_Encoding := 'int'.
	
	FT_String := 'String'.
	FT_Int := 'int'.
	FT_Bitmap_Size := 'FTBitmapSize'.
	FT_CharMap := 'FTCharMapRec*'.
	FT_Generic := 'FTGeneric'.
	FT_BBox := 'FTBBox'.
	FT_GlyphSlot := 'FTGlyphSlotRec*'.
	FT_Driver := 'void*'.
	FT_Memory := 'void*'.
	FT_Stream := 'void*'.
	FT_ListRec := 'FTListRec'.
	FT_Face_Internal := 'void*'.
	FT_Face := 'FTFaceRec*'.
	
	FT_Library := 'void*'.
	FT_Glyph_Metrics := 'FTGlyphMetrics'.
	FT_Fixed := 'long'.
	FT_Vector := 'FTVector'.
	FT_Glyph_Format := 'long'.
	FT_Bitmap := 'FTBitmap'.
	FT_Outline := 'FTOutline'.
	FT_SubGlyph := 'void*'.
	FT_Slot_Internal := 'void*'.
	FT_Size := 'FTSizeRec*'.
	FT_Size_Metrics := 'FTSizeMetrics'.
	FT_Size_Internal := 'void*'.
	
	FT_ListNode := 'void*'.
	FT_Matrix := 'FTMatrix'.
]
