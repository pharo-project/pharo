"
I am a private class, used to represent a 'cairo_font_face_t *' data structure 
of Cairo library.

I am not useful for direct use nor providing any functionality. My only purpose is to keep a strong reference to original FT2Face object
(so it won't be freed before a corresponding instance of mine will release it).


"
Class {
	#name : #CairoFontFace,
	#superclass : #FFIExternalObject,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#pools : [
		'AthensCairoDefinitions',
		'AthensFT2Constants'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #private }
CairoFontFace class >> boldEmphasisCode [

	^ 1
]

{ #category : #finalizing }
CairoFontFace class >> countReferences: handle [
"
unsigned int  cairo_font_face_get_reference_count             (cairo_font_face_t *font_face);
"
	^ self ffiCall: #( uint cairo_font_face_get_reference_count (size_t handle))
]

{ #category : #finalizing }
CairoFontFace class >> finalizeResourceData: handle [

"
void                cairo_font_face_destroy             (cairo_font_face_t *font_face);
"
	^ self ffiCall: #( void cairo_font_face_destroy (size_t handle))
]

{ #category : #'instance creation' }
CairoFontFace class >> fromFreetypeFace: aFace [
	| handle cairoFace |
	handle := aFace getHandle.
 	cairoFace := self primFtFace: handle loadFlags: 0.

	^ cairoFace initializeWithFreetypeFace: aFace
]

{ #category : #finalizing }
CairoFontFace class >> hasSharedResourceData: handle [
	"Answer wether the external cairo_font_face resource is shared (referenced) by other external objects."
	^(self countReferences: handle) > 1
]

{ #category : #private }
CairoFontFace class >> italicEmphasisCode [

	^ 2
]

{ #category : #'instance creation' }
CairoFontFace class >> primFtFace: aFace loadFlags: flags [

"
cairo_font_face_t * cairo_ft_font_face_create_for_ft_face
                                                        (FT_Face face,
                                                         int load_flags);

load_flags: flags to pass to FT_Load_Glyph when loading glyphs from the font. These flags are OR'ed together with the flags derived from the cairo_font_options_t passed to cairo_scaled_font_create(), so only a few values such as FT_LOAD_VERTICAL_LAYOUT, and FT_LOAD_FORCE_AUTOHINT are useful. You should not pass any of the flags affecting the load target, such as FT_LOAD_TARGET_LIGHT.
"
	^ self ffiCall: #( CairoFontFace cairo_ft_font_face_create_for_ft_face(void * aFace , int flags ))
]

{ #category : #initialization }
CairoFontFace >> initialize [
	self autoRelease
]

{ #category : #initialization }
CairoFontFace >> initializeWithFreetypeFace: aFace [
	FFIExternalResourceManager
		addResource: self
		executor:
			(CairoFontFaceExecutor new
				resourceClass: self class data: self resourceData;
				ftFace: aFace)
]

{ #category : #private }
CairoFontFace >> primSetSynthesizeEmphasis: flags [
"
void
cairo_ft_font_face_set_synthesize (cairo_font_face_t *font_face,
                                   int synth_flags);
"
	^ self ffiCall: #( void cairo_ft_font_face_set_synthesize(self, int flags ))
]

{ #category : #'external resource management' }
CairoFontFace >> resourceData [
	^ handle value
]

{ #category : #accessing }
CairoFontFace >> status [
	^ self ffiCall: #(int cairo_font_face_status (self))
]

{ #category : #accessing }
CairoFontFace >> synthesizeBold [
	self primSetSynthesizeEmphasis: 1
]

{ #category : #accessing }
CairoFontFace >> synthesizeBoldAndOblique [
	self primSetSynthesizeEmphasis: 3
]

{ #category : #accessing }
CairoFontFace >> synthesizeEmphasis: emphasisCode [

	emphasisCode = (self class boldEmphasisCode | self class italicEmphasisCode)
		ifTrue: [ ^ self synthesizeBoldAndOblique ].
	emphasisCode = self class boldEmphasisCode
		ifTrue: [ ^ self synthesizeBold ].
	emphasisCode = self class italicEmphasisCode
		ifTrue: [ ^ self synthesizeOblique ]
]

{ #category : #accessing }
CairoFontFace >> synthesizeOblique [
	self primSetSynthesizeEmphasis: 2
]
