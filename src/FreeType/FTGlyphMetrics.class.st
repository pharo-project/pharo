"
I Implement the FT_Glyph_Metrics type
"
Class {
	#name : #FTGlyphMetrics,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_HEIGHT',
		'OFFSET_HORIADVANCE',
		'OFFSET_HORIBEARINGX',
		'OFFSET_HORIBEARINGY',
		'OFFSET_VERTADVANCE',
		'OFFSET_VERTBEARINGX',
		'OFFSET_VERTBEARINGY',
		'OFFSET_WIDTH'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTGlyphMetrics class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(    
	 FT_Pos  width;
    FT_Pos  height;

    FT_Pos  horiBearingX;
    FT_Pos  horiBearingY;
    FT_Pos  horiAdvance;

    FT_Pos  vertBearingX;
    FT_Pos  vertBearingY;
    FT_Pos  vertAdvance;)
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> height [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HEIGHT
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> height: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HEIGHT put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> horiAdvance [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HORIADVANCE
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> horiAdvance: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HORIADVANCE put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> horiBearingX [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HORIBEARINGX
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> horiBearingX: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HORIBEARINGX put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> horiBearingY [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HORIBEARINGY
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> horiBearingY: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_HORIBEARINGY put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> vertAdvance [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_VERTADVANCE
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> vertAdvance: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_VERTADVANCE put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> vertBearingX [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_VERTBEARINGX
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> vertBearingX: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_VERTBEARINGX put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> vertBearingY [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_VERTBEARINGY
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> vertBearingY: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_VERTBEARINGY put: anObject
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> width [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_WIDTH
]

{ #category : #'accessing structure variables' }
FTGlyphMetrics >> width: anObject [
	"This method was automatically generated"
	^handle platformLongAt: OFFSET_WIDTH put: anObject
]
