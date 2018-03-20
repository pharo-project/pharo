"
I map the FT_SizeRec type.
"
Class {
	#name : #FTSizeRec,
	#superclass : #FFIExternalStructure,
	#classVars : [
		'OFFSET_FACE',
		'OFFSET_GENERIC',
		'OFFSET_INTERNAL',
		'OFFSET_METRICS'
	],
	#pools : [
		'FT2Types'
	],
	#category : #'FreeType-Bindings'
}

{ #category : #'field definition' }
FTSizeRec class >> fieldsDesc [

	"self rebuildFieldAccessors"

	^ #(
	 FT_Face           face;
    FT_Generic        generic;
    FT_Size_Metrics   metrics;
    FT_Size_Internal  internal;	
)
]
