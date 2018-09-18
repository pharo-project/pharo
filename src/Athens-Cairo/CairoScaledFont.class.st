"
my instances map to
cairo_scaled_font_t * 
data type
"
Class {
	#name : #CairoScaledFont,
	#superclass : #FFIExternalObject,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'face'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #finalizing }
CairoScaledFont class >> finalizeResourceData: handle [

"
void                cairo_scaled_font_destroy           (cairo_scaled_font_t *scaled_font);
"
	^ self ffiCall: #( void  cairo_scaled_font_destroy (size_t handle) )
]

{ #category : #'instance creation' }
CairoScaledFont class >> fromFreetypeFont: aFont [
	| ftFace face |
	^ CairoBackendCache soleInstance at: aFont ifAbsentPut: [ | emphasis |
			emphasis := aFont simulatedEmphasis.
			ftFace := aFont face.
			face := CairoBackendCache soleInstance 
				at: {ftFace.	emphasis} 
				ifAbsentPut: [ | cff |
					cff := CairoFontFace fromFreetypeFace: ftFace.
					emphasis ifNotNil: [ cff synthesizeEmphasis: emphasis ].
					cff ].
			self fromFreetypeFont: aFont cairoFace: face ]
]

{ #category : #'instance creation' }
CairoScaledFont class >> fromFreetypeFont: aFont cairoFace: face [

	| options fontMatrix deviceMatrix font |

	options := CairoBackendCache soleInstance 
		at: #AAOptions ifAbsentPut: [ CairoFontOptions new ].
	fontMatrix := AthensCairoMatrix new.
	deviceMatrix := AthensCairoMatrix new.

	fontMatrix scaleBy: (TextStyle pointsToPixels: aFont pointSize).
	
	font := self primCreate: face fontMatrix: fontMatrix  userToDeviceMatrix: deviceMatrix options: options. 
	
	"to keep a reference to cairo face in instance"
	^ font initWithFace: face.
	

]

{ #category : #private }
CairoScaledFont class >> primCreate: face fontMatrix: fontMatrix userToDeviceMatrix: ctm options: options [
"
cairo_scaled_font_t * cairo_scaled_font_create          (cairo_font_face_t *font_face,
                                                         const cairo_matrix_t *font_matrix,
                                                         const cairo_matrix_t *ctm,
                                                         const cairo_font_options_t *options);


 Creates a cairo_scaled_font_t object from a font face and matrices that describe the size of the font and the environment in which it will be used.

font_face :
	a cairo_font_face_t

font_matrix :
	font space to user space transformation matrix for the font. In the simplest case of a N point font, this matrix is just a scale by N, but it can also be used to shear the font or stretch it unequally along the two axes. See cairo_set_font_matrix().

ctm :
	user to device transformation matrix with which the font will be used.

options :
	options to use when getting metrics for the font and rendering with it.

Returns :
	a newly created cairo_scaled_font_t. Destroy with cairo_scaled_font_destroy()
"
	^ self ffiCall: #( 
		CairoScaledFont cairo_scaled_font_create (
			CairoFontFace face,
			AthensCairoMatrix * fontMatrix,
			AthensCairoMatrix * ctm,
			CairoFontOptions  options ) )
 
]

{ #category : #accessing }
CairoScaledFont >> extents [
	| extents |
	extents := CairoFontExtents new.

	self getExtents: extents.
	
	^ extents
]

{ #category : #accessing }
CairoScaledFont >> getExtents: cairoFontExtents [
	"void                cairo_scaled_font_extents           (cairo_scaled_font_t *scaled_font,
                                                         cairo_font_extents_t *extents);
	"
	^ self ffiCall: #(int cairo_scaled_font_extents (self, CairoFontExtents * cairoFontExtents ))
		
]

{ #category : #accessing }
CairoScaledFont >> getExtentsOf: utf8String into: extentObj [ 
	"void                cairo_scaled_font_extents           (cairo_scaled_font_t *scaled_font,
                                                         cairo_font_extents_t *extents);
	"
	^ self ffiCall: #(
		void cairo_scaled_font_text_extents (self,
			void *utf8String,
			cairo_text_extents_t * extentObj))
		
]

{ #category : #accessing }
CairoScaledFont >> getExtentsOfGlyphs: glyphs ofLength: num into: extents [ 
	"void
	cairo_scaled_font_glyph_extents (cairo_scaled_font_t *scaled_font,
                                 const cairo_glyph_t *glyphs,
                                 int num_glyphs,
                                 cairo_text_extents_t *extents);"
	^ self ffiCall: #(
		void cairo_scaled_font_glyph_extents (self,
                                            void *glyphs,
                                            int num,
                                            cairo_text_extents_t *extents))
		
]

{ #category : #'initialize-release' }
CairoScaledFont >> initWithFace: cairo_face [

	face := cairo_face.
	
	self autoRelease.
]

{ #category : #'initialize-release' }
CairoScaledFont >> initialize [ 
	super initialize.
	self status ~= CAIRO_STATUS_SUCCESS 
		ifTrue: [ self error: 'Something is wrong!' ].
	self autoRelease
]

{ #category : #locking }
CairoScaledFont >> lock [

"
FT_Face             cairo_ft_scaled_font_lock_face      (cairo_scaled_font_t *scaled_font);

"
	^ self ffiCall: #( 
		void cairo_ft_scaled_font_lock_face (self)) 
]

{ #category : #'external resource management' }
CairoScaledFont >> resourceData [ 
	^ handle value
]

{ #category : #accessing }
CairoScaledFont >> status [
"cairo_status_t      cairo_scaled_font_status            (cairo_scaled_font_t *scaled_font);"
	^ self ffiCall: #(int cairo_scaled_font_status (self))
		
]

{ #category : #locking }
CairoScaledFont >> unlock [

"
FT_Face             cairo_ft_scaled_font_lock_face      (cairo_scaled_font_t *scaled_font);

"
	^ self ffiCall: #( 
		void cairo_ft_scaled_font_unlock_face  (self)) 
]
