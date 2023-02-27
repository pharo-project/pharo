"
I represent a mesh gradient in the Cairo backend.
"
Class {
	#name : #AthensCairoMeshGradientPaint,
	#superclass : #AthensCairoPatternPaint,
	#pools : [
		'AthensCairoDefinitions'
	],
	#category : #'Athens-Cairo-Paints'
}

{ #category : #paints }
AthensCairoMeshGradientPaint class >> createMeshGradientWithPatches: aListOfMeshPatches [
	| paint |
	paint := self primCreateMesh.

	"note, we do #initialize here because instance was created by primitive"
	paint initialize; populatePatches: aListOfMeshPatches.
	^ paint
]

{ #category : #paints }
AthensCairoMeshGradientPaint class >> primCreateMesh [
	^ self ffiCall: #(AthensCairoMeshGradientPaint cairo_pattern_create_mesh () )
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> beginPatch [
	^ self ffiCall:#( void cairo_mesh_pattern_begin_patch ( cairo_pattern_t  self ))
]

{ #category : #accessing }
AthensCairoMeshGradientPaint >> colors: aSequenceOfColors [
	aSequenceOfColors doWithIndex: [ :color :index |
		self setCorner: index - 1 color: color
	]
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> curveVia: pt1 and: lastControlPoint to: endPoint [
	self curveViaX: pt1 x Y: pt1 y viaX: lastControlPoint x Y: lastControlPoint y toX: endPoint x Y: endPoint y
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> curveViaX: x1 Y: y1 viaX: x2 Y: y2 toX: x3 Y: y3 [
	^ self ffiCall: #(void cairo_mesh_pattern_curve_to(cairo_pattern_t self,
                                                         double x1,
                                                         double y1,
                                                         double x2,
                                                         double y2,
                                                         double x3,
                                                         double y3))
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> endPatch [
	^ self ffiCall:#( void cairo_mesh_pattern_end_patch ( cairo_pattern_t  self ))
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> lineTo: aPoint [
	self lineToX: aPoint x asFloat Y: aPoint y asFloat
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> lineToX: x Y: y [
	^ self ffiCall:#( void cairo_mesh_pattern_line_to ( cairo_pattern_t self, double x, double y ))
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> moveTo: aPoint [
	self moveToX: aPoint x asFloat Y: aPoint y asFloat
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> moveToX: x Y: y [
	^ self ffiCall:#( void cairo_mesh_pattern_move_to ( cairo_pattern_t self, double x, double y ))
]

{ #category : #private }
AthensCairoMeshGradientPaint >> populatePatches: aListOfMeshPatches [
	aListOfMeshPatches do: [ :each | each addToCairoMeshGradientPaint: self ]
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> setCorner: corner_num color: color [
	self setCorner: corner_num r: color red g: color green b: color blue a: color alpha
]

{ #category : #'mesh pattern commands' }
AthensCairoMeshGradientPaint >> setCorner: corner_num r: red g: green b: blue a: alpha [
	^ self ffiCall:#( void cairo_mesh_pattern_set_corner_color_rgba ( cairo_pattern_t self, uint corner_num, 	double red, double green, double blue, double alpha ))
]
