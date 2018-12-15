"
my instances representing a low-level cairo context and mapped to cairo_t * C type
"
Class {
	#name : #AthensCairoCanvas,
	#superclass : #AthensCanvas,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'handle',
		'pathTransform',
		'paintTransform',
		'currentClipRect'
	],
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Base'
}

{ #category : #converting }
AthensCairoCanvas class >> asExternalTypeOn: generator [
	"use handle ivar to hold my instance (cairo_t)"
	^ FFIExternalObjectType objectClass: self
]

{ #category : #private }
AthensCairoCanvas class >> on: cairoSurface [

	^ (self primCreate: cairoSurface) surface: cairoSurface
]

{ #category : #primitives }
AthensCairoCanvas class >> primCreate: cairoSurface [
	^ self ffiCall: #( 
		AthensCairoCanvas cairo_create (AthensCairoSurface cairoSurface) )
]

{ #category : #clipping }
AthensCairoCanvas >> clipBy: aRectangle during: aBlock [
	| oldClip newClip |

	oldClip := currentClipRect.
	newClip := oldClip ifNil: [ aRectangle ] ifNotNil: [ oldClip intersect: aRectangle ifNone:[^ self] ]. 
	
	self setPathMatrix; resetClip; setClipRect: newClip.
	
	currentClipRect := newClip.

	[aBlock value] ensure: [ 
		self 
		setPathMatrix;
		resetClip;
		setClipRect: oldClip. 
		currentClipRect := oldClip. ].
	

]

{ #category : #accessing }
AthensCairoCanvas >> clipRect [
   ^ currentClipRect
        ifNil: [ pathTransform
                ifNotNil: [ (pathTransform inverted transform: 0 @ 0) extent: self surface extent ]
                ifNil: (0 @ 0 extent: self surface extent) ]
]

{ #category : #private }
AthensCairoCanvas >> destroy [
	^ self ffiCall: #( 	void cairo_destroy (self) )
]

{ #category : #drawing }
AthensCairoCanvas >> draw [
	"Fill the shape (anObject) using currently selected paint
	an object should implement double-dispatch to currently selected paint"
	
	"set the trasformation matrix"
	self setPathMatrix.
	
	^ shape paintFillsUsing: paint on: self


]

{ #category : #drawing }
AthensCairoCanvas >> drawWithAlpha: alpha [
	^ self ffiCall: #( 
		void cairo_paint_with_alpha ( self , double alpha)
	)
]

{ #category : #private }
AthensCairoCanvas >> fill [
	^ self ffiCall: #(void cairo_fill (self))

]

{ #category : #private }
AthensCairoCanvas >> getCurrentPoint [

	| x y |
	"since we use pointers to floats we must create a copy of original values to not scratch them"
	x := ByteArray new: 8.
	y := ByteArray new: 8.
	self primGetCurrentPointX: x Y: y.
	^ (x doubleAt: 1)@ (y doubleAt: 1)

]

{ #category : #private }
AthensCairoCanvas >> getScaledFont [
	"Returns the current scaled font. This object is owned by cairo. To keep a reference to it, you must call cairo_scaled_font_reference().
 "
	^ self ffiCall: #( CairoScaledFont cairo_get_scaled_font ( self ))
]

{ #category : #accessing }
AthensCairoCanvas >> handle [
	^ handle value
]

{ #category : #'initialize-release' }
AthensCairoCanvas >> initializeWithSurface: anAthensSurface [	
	super initializeWithSurface: anAthensSurface.
	self paintMode default.
]

{ #category : #private }
AthensCairoCanvas >> loadPath: aPath [
	^ self ffiCall: #( void cairo_append_path (self , AthensCairoPath aPath))

]

{ #category : #private }
AthensCairoCanvas >> moveToX: x Y: y [
	" move command always starts a new contour "
	^ self ffiCall: #(void cairo_move_to (self, double x, double y ) )

]

{ #category : #private }
AthensCairoCanvas >> newPath [
	^ self ffiCall: #( void cairo_new_path ( self ) )
]

{ #category : #accessing }
AthensCairoCanvas >> paintTransform [
	^ paintTransform

]

{ #category : #accessing }
AthensCairoCanvas >> pathTransform [
	^ pathTransform
]

{ #category : #private }
AthensCairoCanvas >> primClip [
	^ self ffiCall: #(void cairo_clip (self))

]

{ #category : #private }
AthensCairoCanvas >> primGetCurrentPointX: x Y: y [
	^ self ffiCall: #( void cairo_get_current_point (self, double * x, double * y))
]

{ #category : #private }
AthensCairoCanvas >> primGetSource [
	^ self ffiCall: #( cairo_pattern_t cairo_get_source ( self ) )
]

{ #category : #private }
AthensCairoCanvas >> primPaint [
	"A drawing operator that paints the current source everywhere within the current clip region."
	^ self ffiCall: #(void cairo_paint (self))

]

{ #category : #private }
AthensCairoCanvas >> primResetDashes: anOffset [
"	void                cairo_set_dash                      (cairo_t *cr,
                                                         double *dashes,
                                                         int num_dashes,
                                                         double offset);"
	^ self ffiCall: #(void cairo_set_dash (
				self,
				0,
				0,
				double anOffset) )

]

{ #category : #private }
AthensCairoCanvas >> primSetDashesLengths: dashesLengths count: dashesCount offset: anOffset [
	^ self ffiCall: #(void cairo_set_dash (self,
			double * dashesLengths,
			int dashesCount,
			double anOffset ) )
]

{ #category : #private }
AthensCairoCanvas >> primSetLineCap: capStyle [
	^ self ffiCall: #(void cairo_set_line_cap ( self, cairo_line_cap_t capStyle ) )
]

{ #category : #private }
AthensCairoCanvas >> primSetLineJoin: joinStyle [
	^ self ffiCall: #(void cairo_set_line_join ( self, cairo_line_join_t joinStyle) )
]

{ #category : #private }
AthensCairoCanvas >> primSetLineWidth: width [
	^ self ffiCall: #(void cairo_set_line_width (self, double width) )
]

{ #category : #private }
AthensCairoCanvas >> primSetSource: aPattern [
	^ self ffiCall: #(void cairo_set_source ( self, AthensCairoPatternPaint aPattern))
]

{ #category : #private }
AthensCairoCanvas >> rectangleX: x y: y width: aWidth height: aHeight [
	^ self ffiCall: #(void cairo_rectangle (self,
		 double x, double y,
		 double aWidth, double aHeight) )

]

{ #category : #private }
AthensCairoCanvas >> resetClip [
	^ self ffiCall: #(void cairo_reset_clip (self))

]

{ #category : #private }
AthensCairoCanvas >> resetDash [
	self primResetDashes: 0.0
]

{ #category : #private }
AthensCairoCanvas >> setAA: antiAlias [

	"cairo_set_antialias ()

void                cairo_set_antialias                 (cairo_t *cr,
                                                         cairo_antialias_t antialias);

Set the antialiasing mode of the rasterizer used for drawing shapes. This value is a hint, and a particular backend may or may not support a particular value. At the current time, no backend supports CAIRO_ANTIALIAS_SUBPIXEL when drawing shapes.

Note that this option does not affect text rendering, instead see cairo_font_options_set_antialias().

"
	^ self ffiCall: #(void cairo_set_antialias (self, uint antiAlias))

]

{ #category : #private }
AthensCairoCanvas >> setClipRect: aRectOrNil [
	

	aRectOrNil 
		ifNil: [ self resetClip ]
		ifNotNil: [
			self newPath;
				rectangleX: aRectOrNil left
				y: aRectOrNil top
				width: aRectOrNil width
				height: aRectOrNil height;
				primClip
			]
]

{ #category : #private }
AthensCairoCanvas >> setFontMatrix: aMatrix [
	"
void                cairo_set_font_matrix               (cairo_t *cr,
                                                         const cairo_matrix_t *matrix);
	"
	^ self ffiCall: #( void cairo_set_font_matrix (self , AthensCairoMatrix  * aMatrix ))

]

{ #category : #private }
AthensCairoCanvas >> setFontSize: aSize [
	"
	void                cairo_set_font_size                 (cairo_t *cr,
                                                         double size);
	"
	^ self ffiCall: #( void cairo_set_font_size (self , double aSize ))

]

{ #category : #private }
AthensCairoCanvas >> setPathMatrix [
	^ self ffiCall: #(void cairo_set_matrix (self,
			AthensCairoMatrix * pathTransform) )

]

{ #category : #private }
AthensCairoCanvas >> setPathMatrix: aMatrix [
	^ self ffiCall: #(void cairo_set_matrix (self,
			AthensCairoMatrix * aMatrix) )

]

{ #category : #private }
AthensCairoCanvas >> setScaledFont: aFont [
	"Replaces the current font face, font matrix, and font options in the cairo_t with those of the cairo_scaled_font_t. Except for some translation, the current CTM of the cairo_t should be the same as that of the cairo_scaled_font_t, which can be accessed using cairo_scaled_font_get_ctm().
 "
	^ self ffiCall: #( void cairo_set_scaled_font (self , CairoScaledFont aFont))

]

{ #category : #private }
AthensCairoCanvas >> setSourceR: red g: green b: blue a: alpha [
	^ self ffiCall: #( 
		void cairo_set_source_rgba ( self ,
			double red,
			double green,
			double blue,
			double alpha) )
]

{ #category : #private }
AthensCairoCanvas >> showGlyphs: glyphs size: numGlyphs [
	"A drawing operator that generates the shape from a string of UTF-8 characters, rendered according to the current font_face, font_size (font_matrix), and font_options. "
	^ self ffiCall: #(
	
		void cairo_show_glyphs ( self ,
			void * glyphs,
			int numGlyphs) )

]

{ #category : #private }
AthensCairoCanvas >> showText: anUTF8String [
	"A drawing operator that generates the shape from a string of UTF-8 characters, rendered according to the current font_face, font_size (font_matrix), and font_options. "
	^ self ffiCall: #(void cairo_show_text (self, char * anUTF8String ))

]

{ #category : #private }
AthensCairoCanvas >> stroke [
	^ self ffiCall: #(void cairo_stroke (self))

]

{ #category : #accessing }
AthensCairoCanvas >> surface: aCairoSurface [
	
	"initialize for given surface"
	surface := aCairoSurface.
	
	pathTransform := AthensCairoMatrix new.
	paintTransform := AthensCairoMatrix new.
	paintMode := AthensCairoPaintMode new canvas: self.
	
	self setAA: CAIRO_ANTIALIAS_SUBPIXEL.
]

{ #category : #private }
AthensCairoCanvas >> text: utf8String extents: extentsObj [ 
"cairo_text_extents ()

void                cairo_text_extents                  (cairo_t *cr,
                                                         const char *utf8,
                                                         cairo_text_extents_t *extents);
"
	^ self ffiCall: #( void cairo_text_extents (self, char * utf8String, cairo_text_extents_t * extentsObj) )
	
]

{ #category : #'path segments visitor' }
AthensCairoCanvas >> visitCloseSegment: closeSegment [
	self closePath
]

{ #category : #'path segments visitor' }
AthensCairoCanvas >> visitCubicSegment: anAthensCubicSegment [
	|  destination controlPoint controlPoint2 |
	destination := anAthensCubicSegment to.
	controlPoint := anAthensCubicSegment via1.
	controlPoint2 := anAthensCubicSegment via2.
	self
		curveToX: destination x Y: destination y
		viaX: controlPoint x Y: controlPoint y
		andX: controlPoint2 x Y: controlPoint2 y
		 		

]

{ #category : #'path segments visitor' }
AthensCairoCanvas >> visitLineSegment: line [

	^ self lineToX: line endPoint x Y: line endPoint y
]

{ #category : #'path segments visitor' }
AthensCairoCanvas >> visitMoveSegment: mov [ 
	^ self moveToX: mov endPoint x Y: mov endPoint y
]

{ #category : #'path segments visitor' }
AthensCairoCanvas >> visitQuadSegment: anAthensCubicSegment [
	| destination controlPoint |
	destination := anAthensCubicSegment to.
	controlPoint := anAthensCubicSegment via.
	self
		curveToX: destination x Y: destination y
		viaX: controlPoint x Y: controlPoint y
		 
]
