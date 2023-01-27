"
I hold a lot of different constants needed to work with cairo library
"
Class {
	#name : #AthensCairoDefinitions,
	#superclass : #SharedPool,
	#classVars : [
		'CAIRO_ANTIALIAS_BEST',
		'CAIRO_ANTIALIAS_DEFAULT',
		'CAIRO_ANTIALIAS_FAST',
		'CAIRO_ANTIALIAS_GOOD',
		'CAIRO_ANTIALIAS_GRAY',
		'CAIRO_ANTIALIAS_NONE',
		'CAIRO_ANTIALIAS_SUBPIXEL',
		'CAIRO_EXTEND_NONE',
		'CAIRO_EXTEND_PAD',
		'CAIRO_EXTEND_REFLECT',
		'CAIRO_EXTEND_REPEAT',
		'CAIRO_FILL_RULE_EVEN_ODD',
		'CAIRO_FILL_RULE_WINDING',
		'CAIRO_FONT_SLANT_ITALIC',
		'CAIRO_FONT_SLANT_NORMAL',
		'CAIRO_FONT_SLANT_OBLIQUE',
		'CAIRO_FONT_TYPE_FT',
		'CAIRO_FONT_TYPE_QUARTZ',
		'CAIRO_FONT_TYPE_TOY',
		'CAIRO_FONT_TYPE_USER',
		'CAIRO_FONT_TYPE_WIN32',
		'CAIRO_FONT_WEIGHT_BOLD',
		'CAIRO_FONT_WEIGHT_NORMAL',
		'CAIRO_FORMAT_A1',
		'CAIRO_FORMAT_A8',
		'CAIRO_FORMAT_ARGB32',
		'CAIRO_FORMAT_INVALID',
		'CAIRO_FORMAT_RGB16_565',
		'CAIRO_FORMAT_RGB24',
		'CAIRO_HINT_METRICS_DEFAULT',
		'CAIRO_HINT_METRICS_OFF',
		'CAIRO_HINT_METRICS_ON',
		'CAIRO_HINT_STYLE_DEFAULT',
		'CAIRO_HINT_STYLE_FULL',
		'CAIRO_HINT_STYLE_MEDIUM',
		'CAIRO_HINT_STYLE_NONE',
		'CAIRO_HINT_STYLE_SLIGHT',
		'CAIRO_LINE_CAP_BUTT',
		'CAIRO_LINE_CAP_ROUND',
		'CAIRO_LINE_CAP_SQUARE',
		'CAIRO_LINE_JOIN_BEVEL',
		'CAIRO_LINE_JOIN_MITER',
		'CAIRO_LINE_JOIN_ROUND',
		'CAIRO_OPERATOR_ADD',
		'CAIRO_OPERATOR_ATOP',
		'CAIRO_OPERATOR_CLEAR',
		'CAIRO_OPERATOR_COLOR_BURN',
		'CAIRO_OPERATOR_COLOR_DODGE',
		'CAIRO_OPERATOR_DARKEN',
		'CAIRO_OPERATOR_DEST',
		'CAIRO_OPERATOR_DEST_ATOP',
		'CAIRO_OPERATOR_DEST_IN',
		'CAIRO_OPERATOR_DEST_OUT',
		'CAIRO_OPERATOR_DEST_OVER',
		'CAIRO_OPERATOR_DIFFERENCE',
		'CAIRO_OPERATOR_EXCLUSION',
		'CAIRO_OPERATOR_HARD_LIGHT',
		'CAIRO_OPERATOR_HSL_COLOR',
		'CAIRO_OPERATOR_HSL_HUE',
		'CAIRO_OPERATOR_HSL_LUMINOSITY',
		'CAIRO_OPERATOR_HSL_SATURATION',
		'CAIRO_OPERATOR_IN',
		'CAIRO_OPERATOR_LIGHTEN',
		'CAIRO_OPERATOR_MULTIPLY',
		'CAIRO_OPERATOR_OUT',
		'CAIRO_OPERATOR_OVER',
		'CAIRO_OPERATOR_OVERLAY',
		'CAIRO_OPERATOR_SATURATE',
		'CAIRO_OPERATOR_SCREEN',
		'CAIRO_OPERATOR_SOFT_LIGHT',
		'CAIRO_OPERATOR_SOURCE',
		'CAIRO_OPERATOR_XOR',
		'CAIRO_STATUS_CLIP_NOT_REPRESENTABLE',
		'CAIRO_STATUS_DEVICE_ERROR',
		'CAIRO_STATUS_DEVICE_TYPE_MISMATCH',
		'CAIRO_STATUS_FILE_NOT_FOUND',
		'CAIRO_STATUS_FONT_TYPE_MISMATCH',
		'CAIRO_STATUS_INVALID_CLUSTERS',
		'CAIRO_STATUS_INVALID_CONTENT',
		'CAIRO_STATUS_INVALID_DASH',
		'CAIRO_STATUS_INVALID_DSC_COMMENT',
		'CAIRO_STATUS_INVALID_FORMAT',
		'CAIRO_STATUS_INVALID_INDEX',
		'CAIRO_STATUS_INVALID_MATRIX',
		'CAIRO_STATUS_INVALID_PATH_DATA',
		'CAIRO_STATUS_INVALID_POP_GROUP',
		'CAIRO_STATUS_INVALID_RESTORE',
		'CAIRO_STATUS_INVALID_SIZE',
		'CAIRO_STATUS_INVALID_SLANT',
		'CAIRO_STATUS_INVALID_STATUS',
		'CAIRO_STATUS_INVALID_STRIDE',
		'CAIRO_STATUS_INVALID_STRING',
		'CAIRO_STATUS_INVALID_VISUAL',
		'CAIRO_STATUS_INVALID_WEIGHT',
		'CAIRO_STATUS_LAST_STATUS',
		'CAIRO_STATUS_NEGATIVE_COUNT',
		'CAIRO_STATUS_NO_CURRENT_POINT',
		'CAIRO_STATUS_NO_MEMORY',
		'CAIRO_STATUS_NULL_POINTER',
		'CAIRO_STATUS_PATTERN_TYPE_MISMATCH',
		'CAIRO_STATUS_READ_ERROR',
		'CAIRO_STATUS_SUCCESS',
		'CAIRO_STATUS_SURFACE_FINISHED',
		'CAIRO_STATUS_SURFACE_TYPE_MISMATCH',
		'CAIRO_STATUS_TEMP_FILE_ERROR',
		'CAIRO_STATUS_USER_FONT_ERROR',
		'CAIRO_STATUS_USER_FONT_IMMUTABLE',
		'CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED',
		'CAIRO_STATUS_WRITE_ERROR',
		'CAIRO_SUBPIXEL_ORDER_BGR',
		'CAIRO_SUBPIXEL_ORDER_DEFAULT',
		'CAIRO_SUBPIXEL_ORDER_RGB',
		'CAIRO_SUBPIXEL_ORDER_VBGR',
		'CAIRO_SUBPIXEL_ORDER_VRGB',
		'cairo_fill_rule_t',
		'cairo_font_slant_t',
		'cairo_font_type_t',
		'cairo_font_weight_t',
		'cairo_line_cap_t',
		'cairo_line_join_t',
		'cairo_operator_t',
		'cairo_pattern_t',
		'cairo_status_t',
		'cairo_surface_t',
		'cairo_t',
		'cairo_text_extents_t'
	],
	#category : #'Athens-Cairo-Pools'
}

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> init_cairo_subpixel_order_t [

"typedef enum {"
    CAIRO_SUBPIXEL_ORDER_DEFAULT := 0.
    CAIRO_SUBPIXEL_ORDER_RGB := 1.
    CAIRO_SUBPIXEL_ORDER_BGR := 2.
    CAIRO_SUBPIXEL_ORDER_VRGB := 3.
    CAIRO_SUBPIXEL_ORDER_VBGR := 4.
"} cairo_subpixel_order_t;"
]

{ #category : #'class initialization' }
AthensCairoDefinitions class >> initialize [

	"
	self initialize
	"

	self
		initialize_types;
		initialize_cairo_format_t;
		initialize_cairo_font_slant_t;
		initialize_cairo_font_weight_t;
		initialize_cairo_font_type_t ;
		initialize_cairo_operator_t ;
		initialize_cairo_line_cap_t ;
		initialize_cairo_line_join_t;
		initialize_cairo_status_t;
		initialize_cairo_extend_t;
		initialize_cairo_hint_style_t;
		initialize_cairo_antialias_t;
		init_cairo_subpixel_order_t;
		initialize_cairo_hint_metrics_t;
		initialize_cairo_fill_rule_t
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_antialias_t [

"typedef enum {"
    CAIRO_ANTIALIAS_DEFAULT := 0.

    " method "
    CAIRO_ANTIALIAS_NONE := 1.
    CAIRO_ANTIALIAS_GRAY := 2.
    CAIRO_ANTIALIAS_SUBPIXEL := 3.

    " hints "
    CAIRO_ANTIALIAS_FAST := 4.
    CAIRO_ANTIALIAS_GOOD := 5.
    CAIRO_ANTIALIAS_BEST := 6.
"} cairo_antialias_t; "
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_extend_t [
"typedef enum {"
    CAIRO_EXTEND_NONE := 0 .
    CAIRO_EXTEND_REPEAT := 1.
    CAIRO_EXTEND_REFLECT :=2.
    CAIRO_EXTEND_PAD := 3.
"} cairo_extend_t"
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_fill_rule_t [
"	typedef enum _cairo_status {"
   CAIRO_FILL_RULE_WINDING := 0.
	CAIRO_FILL_RULE_EVEN_ODD := 1
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_font_slant_t [

	"typedef enum _cairo_font_slant {"
    CAIRO_FONT_SLANT_NORMAL := 0.
    CAIRO_FONT_SLANT_ITALIC := 1.
    CAIRO_FONT_SLANT_OBLIQUE := 2
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_font_type_t [
	"typedef enum _cairo_font_type {"
    CAIRO_FONT_TYPE_TOY := 0.
    CAIRO_FONT_TYPE_FT := 1.
    CAIRO_FONT_TYPE_WIN32 := 2.
    CAIRO_FONT_TYPE_QUARTZ := 3.
    CAIRO_FONT_TYPE_USER := 4
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_font_weight_t [

	"typedef enum _cairo_font_weight {"
    CAIRO_FONT_WEIGHT_NORMAL := 0.
    CAIRO_FONT_WEIGHT_BOLD := 1
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_format_t [

	"enum cairo_format_t"
	CAIRO_FORMAT_INVALID :=  -1.
	CAIRO_FORMAT_ARGB32 :=  0.
	CAIRO_FORMAT_RGB24 := 1.
	CAIRO_FORMAT_A8 := 2.
	CAIRO_FORMAT_A1 := 3.
	CAIRO_FORMAT_RGB16_565 := 4
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_hint_metrics_t [
"typedef enum {"
    CAIRO_HINT_METRICS_DEFAULT := 0.
    CAIRO_HINT_METRICS_OFF := 1.
    CAIRO_HINT_METRICS_ON := 2
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_hint_style_t [

"typedef enum { "
    CAIRO_HINT_STYLE_DEFAULT := 0.
    CAIRO_HINT_STYLE_NONE := 1.
    CAIRO_HINT_STYLE_SLIGHT := 2.
    CAIRO_HINT_STYLE_MEDIUM := 3.
    CAIRO_HINT_STYLE_FULL := 4.
"} cairo_hint_style_t; "
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_line_cap_t [
	"typedef enum _cairo_line_cap { "
    CAIRO_LINE_CAP_BUTT := 0.
    CAIRO_LINE_CAP_ROUND := 1.
    CAIRO_LINE_CAP_SQUARE := 2
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_line_join_t [
"	typedef enum _cairo_line_join {"
    CAIRO_LINE_JOIN_MITER := 0.
    CAIRO_LINE_JOIN_ROUND := 1.
    CAIRO_LINE_JOIN_BEVEL := 2
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_operator_t [
"	typedef enum _cairo_operator {"
    CAIRO_OPERATOR_CLEAR := 0.

    CAIRO_OPERATOR_SOURCE:= 1.
    CAIRO_OPERATOR_OVER:= 2.
    CAIRO_OPERATOR_IN:= 3.
    CAIRO_OPERATOR_OUT:= 4.
    CAIRO_OPERATOR_ATOP:= 5.

    CAIRO_OPERATOR_DEST:= 6.
    CAIRO_OPERATOR_DEST_OVER:= 7.
    CAIRO_OPERATOR_DEST_IN:= 8.
    CAIRO_OPERATOR_DEST_OUT:= 9.
    CAIRO_OPERATOR_DEST_ATOP:= 10.

    CAIRO_OPERATOR_XOR:= 11.
    CAIRO_OPERATOR_ADD:= 12.
    CAIRO_OPERATOR_SATURATE:= 13.

    CAIRO_OPERATOR_MULTIPLY:= 14.
    CAIRO_OPERATOR_SCREEN:= 15.
    CAIRO_OPERATOR_OVERLAY:= 16.
    CAIRO_OPERATOR_DARKEN := 17.
    CAIRO_OPERATOR_LIGHTEN := 18.
    CAIRO_OPERATOR_COLOR_DODGE := 19.
    CAIRO_OPERATOR_COLOR_BURN := 20.
    CAIRO_OPERATOR_HARD_LIGHT := 21.
    CAIRO_OPERATOR_SOFT_LIGHT := 22.
    CAIRO_OPERATOR_DIFFERENCE := 23.
    CAIRO_OPERATOR_EXCLUSION := 24.
    CAIRO_OPERATOR_HSL_HUE := 25.
    CAIRO_OPERATOR_HSL_SATURATION := 26.
    CAIRO_OPERATOR_HSL_COLOR := 27.
    CAIRO_OPERATOR_HSL_LUMINOSITY := 28.
"} cairo_operator_t;"
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_cairo_status_t [
"	typedef enum _cairo_status {"
    CAIRO_STATUS_SUCCESS := 0.

    CAIRO_STATUS_NO_MEMORY:=1.
    CAIRO_STATUS_INVALID_RESTORE:=2.
    CAIRO_STATUS_INVALID_POP_GROUP:=3.
    CAIRO_STATUS_NO_CURRENT_POINT:=4.
    CAIRO_STATUS_INVALID_MATRIX:=5.
    CAIRO_STATUS_INVALID_STATUS:=6.
    CAIRO_STATUS_NULL_POINTER:=7.
    CAIRO_STATUS_INVALID_STRING:=8.
    CAIRO_STATUS_INVALID_PATH_DATA:=9.
    CAIRO_STATUS_READ_ERROR:= 10.
    CAIRO_STATUS_WRITE_ERROR:=11.
    CAIRO_STATUS_SURFACE_FINISHED:=12.
    CAIRO_STATUS_SURFACE_TYPE_MISMATCH:=13.
    CAIRO_STATUS_PATTERN_TYPE_MISMATCH:=14.
    CAIRO_STATUS_INVALID_CONTENT:=15.
    CAIRO_STATUS_INVALID_FORMAT:=16.
    CAIRO_STATUS_INVALID_VISUAL:=17.
    CAIRO_STATUS_FILE_NOT_FOUND:=18.
    CAIRO_STATUS_INVALID_DASH:=19.
    CAIRO_STATUS_INVALID_DSC_COMMENT:=20.
    CAIRO_STATUS_INVALID_INDEX:=21.
    CAIRO_STATUS_CLIP_NOT_REPRESENTABLE:=22.
    CAIRO_STATUS_TEMP_FILE_ERROR:=23.
    CAIRO_STATUS_INVALID_STRIDE:=24.
    CAIRO_STATUS_FONT_TYPE_MISMATCH:=25.
    CAIRO_STATUS_USER_FONT_IMMUTABLE:=26.
    CAIRO_STATUS_USER_FONT_ERROR:=27.
    CAIRO_STATUS_NEGATIVE_COUNT:=28.
    CAIRO_STATUS_INVALID_CLUSTERS:=29.
    CAIRO_STATUS_INVALID_SLANT:=30.
    CAIRO_STATUS_INVALID_WEIGHT:=31.
    CAIRO_STATUS_INVALID_SIZE:=32.
    CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED:=33.
    CAIRO_STATUS_DEVICE_TYPE_MISMATCH:=34.
    CAIRO_STATUS_DEVICE_ERROR:=35
]

{ #category : #'private - initialization' }
AthensCairoDefinitions class >> initialize_types [

	cairo_status_t := cairo_font_type_t :=  cairo_line_join_t  := cairo_line_cap_t := cairo_status_t := cairo_font_slant_t := cairo_font_weight_t := cairo_operator_t  := cairo_fill_rule_t := #int.

	cairo_pattern_t := #AthensCairoPatternPaint.

	cairo_t := #AthensCairoCanvas.

	cairo_surface_t := #AthensCairoSurface .
	cairo_text_extents_t := #CairoTextExtents
]
