"
i represent various font options exposed by cairo library
"
Class {
	#name : #CairoFontOptions,
	#superclass : #FFIExternalObject,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #finalizing }
CairoFontOptions class >> finalizeResourceData: aData [
	"finalize the external resource data, returned by #resourceData on instance side"

	self primDestroy: aData
]

{ #category : #'instance creation' }
CairoFontOptions class >> new [
	^ self primCreate initialize
]

{ #category : #private }
CairoFontOptions class >> primCreate [
"cairo_font_options_t * cairo_font_options_create        (void);

Allocates a new font options object with all options initialized to default values.

Returns :
	a newly allocated cairo_font_options_t. Free with cairo_font_options_destroy(). This function always returns a valid pointer; if memory cannot be allocated, then a special error object is returned where all operations on the object do nothing. You can check for this with cairo_font_options_status().

"
	^ self ffiCall: #(CairoFontOptions cairo_font_options_create ())
]

{ #category : #private }
CairoFontOptions class >> primDestroy: anOptions [
"void                cairo_font_options_destroy          (cairo_font_options_t *options);

Destroys a cairo_font_options_t object created with cairo_font_options_create() or cairo_font_options_copy().

options :
	a cairo_font_options_t"
	^ self ffiCall: #(void cairo_font_options_destroy (size_t anOptions))
]

{ #category : #antialiasing }
CairoFontOptions >> aaBest [

	^ self setAA: CAIRO_ANTIALIAS_BEST
]

{ #category : #antialiasing }
CairoFontOptions >> aaDefault [

	^ self setAA: CAIRO_ANTIALIAS_DEFAULT
]

{ #category : #antialiasing }
CairoFontOptions >> aaFast [

	^ self setAA: CAIRO_ANTIALIAS_FAST
]

{ #category : #antialiasing }
CairoFontOptions >> aaGood [

	^ self setAA: CAIRO_ANTIALIAS_GOOD
]

{ #category : #antialiasing }
CairoFontOptions >> aaGray [

	^ self setAA: CAIRO_ANTIALIAS_GRAY
]

{ #category : #antialiasing }
CairoFontOptions >> aaNone [

	^ self setAA: CAIRO_ANTIALIAS_NONE
]

{ #category : #antialiasing }
CairoFontOptions >> aaSubpixel [

	^ self setAA: CAIRO_ANTIALIAS_SUBPIXEL
]

{ #category : #'hint options' }
CairoFontOptions >> hintMetricsDefault [
	^ self setHintMetrics: CAIRO_HINT_METRICS_DEFAULT
]

{ #category : #'hint options' }
CairoFontOptions >> hintMetricsOff [
	^ self setHintMetrics: CAIRO_HINT_METRICS_OFF
]

{ #category : #'hint options' }
CairoFontOptions >> hintMetricsOn [
	^ self setHintMetrics: CAIRO_HINT_METRICS_ON
]

{ #category : #'hint options' }
CairoFontOptions >> hintStyleDefault [
	^ self setHintStyle: CAIRO_HINT_STYLE_DEFAULT
]

{ #category : #'hint options' }
CairoFontOptions >> hintStyleFull [
	^ self setHintStyle: CAIRO_HINT_STYLE_FULL
]

{ #category : #'hint options' }
CairoFontOptions >> hintStyleMedium [
	^ self setHintStyle: CAIRO_HINT_STYLE_MEDIUM
]

{ #category : #'hint options' }
CairoFontOptions >> hintStyleNone [
	^ self setHintStyle: CAIRO_HINT_STYLE_NONE
]

{ #category : #'hint options' }
CairoFontOptions >> hintStyleSlight [
	^ self setHintStyle: CAIRO_HINT_STYLE_SLIGHT
]

{ #category : #initialization }
CairoFontOptions >> initialize [
	self autoRelease.
	self setDefaults
]

{ #category : #initialization }
CairoFontOptions >> resourceData [

	^ handle value
]

{ #category : #antialiasing }
CairoFontOptions >> setAA: antialias [

"cairo_font_options_set_antialias ()

void                cairo_font_options_set_antialias    (cairo_font_options_t *options,
                                                         cairo_antialias_t antialias);

Sets the antialiasing mode for the font options object. This specifies the type of antialiasing to do when rendering text.

options :
	a cairo_font_options_t

antialias :
	the new antialiasing mode"
	^ self ffiCall: #(
	void cairo_font_options_set_antialias (self, uint antialias))
]

{ #category : #defaults }
CairoFontOptions >> setDefaults [

	self hintStyleSlight; hintMetricsOff; subpixelOrderRGB; aaSubpixel
]

{ #category : #'hint options' }
CairoFontOptions >> setHintMetrics: hintMetrics [
"
	void	cairo_font_options_set_hint_metrics (cairo_font_options_t *options,
		cairo_hint_metrics_t hint_metrics);

 Sets the metrics hinting mode for the font options object. This controls whether metrics are quantized to integer values in device units. See the documentation for cairo_hint_metrics_t for full details.

"
	^ self ffiCall: #(void cairo_font_options_set_hint_metrics (self,  uint hintMetrics))
]

{ #category : #'hint options' }
CairoFontOptions >> setHintStyle: aStyle [
	"cairo_font_options_set_hint_style ()

void                cairo_font_options_set_hint_style   (cairo_font_options_t *options,
                                                         cairo_hint_style_t hint_style);

Sets the hint style for font outlines for the font options object. This controls whether to fit font outlines to the pixel grid, and if so, whether to optimize for fidelity or contrast. See the documentation for cairo_hint_style_t for full details. "
	^ self ffiCall: #(
		void cairo_font_options_set_hint_style (self, uint aStyle))
]

{ #category : #'subpixel order' }
CairoFontOptions >> setSubpixelOrder: order [
	"void cairo_font_options_set_subpixel_order
		(cairo_font_options_t *options,
		cairo_subpixel_order_t subpixel_order)"
	^ self ffiCall: #(
		void cairo_font_options_set_subpixel_order (self, uint order))
]

{ #category : #'subpixel order' }
CairoFontOptions >> subpixelOrderBGR [
	^ self setSubpixelOrder: CAIRO_SUBPIXEL_ORDER_BGR
]

{ #category : #'subpixel order' }
CairoFontOptions >> subpixelOrderDefault [
	^ self setSubpixelOrder: CAIRO_SUBPIXEL_ORDER_DEFAULT
]

{ #category : #'subpixel order' }
CairoFontOptions >> subpixelOrderRGB [
	^ self setSubpixelOrder: CAIRO_SUBPIXEL_ORDER_RGB
]

{ #category : #'subpixel order' }
CairoFontOptions >> subpixelOrderVBGR [
	^ self setSubpixelOrder: CAIRO_SUBPIXEL_ORDER_VBGR
]

{ #category : #'subpixel order' }
CairoFontOptions >> subpixelOrderVRGB [
	^ self setSubpixelOrder: CAIRO_SUBPIXEL_ORDER_VRGB
]
