"
I am a gradient that is defined by the composition of multiple patches.
"
Class {
	#name : #MeshGradientPaint,
	#superclass : #AthensAbstractPaint,
	#instVars : [
		'patches'
	],
	#category : #'Athens-Core-Paints'
}

{ #category : #adding }
MeshGradientPaint >> addCircleSliceCenter: center radius: radius startAngle: sliceStartAngle endAngle: sliceEndAngle colors: colors [
	| sliceStartPoint sliceEndPoint f sliceControlPoint1 sliceControlPoint2 sliceCenter |
	sliceStartPoint := Point r: radius theta: sliceStartAngle.
	sliceEndPoint := Point r: radius theta: sliceEndAngle.

"Circle subdivision algorithm from: https://blogs.igalia.com/dpino/2020/06/11/renderization-of-conic-gradients/ [October, 2020]"
	f := ( ((sliceEndAngle - sliceStartAngle) / 4) tan) * 4 / 3.
	sliceControlPoint1 := (sliceStartPoint x - (f * sliceStartPoint y)) @ (sliceStartPoint y + (f * sliceStartPoint x)).
	sliceControlPoint2 := (sliceEndPoint x + (f * sliceEndPoint y)) @ (sliceEndPoint y - (f * sliceEndPoint x)).

	"Transform the control points to the actual center."
	sliceCenter := center.
	sliceStartPoint := sliceStartPoint + center.
	sliceControlPoint1 := sliceControlPoint1 + center.
	sliceControlPoint2 := sliceControlPoint2 + center.
	sliceEndPoint := sliceEndPoint + center.

	self
		buildPatchWith: [ :patchBuilder |
			patchBuilder
				moveTo: sliceCenter;
				lineTo: sliceStartPoint;
				curveVia: sliceControlPoint1 and: sliceControlPoint2 to: sliceEndPoint;
				lineTo: sliceCenter;
				colors: colors
		]
]

{ #category : #adding }
MeshGradientPaint >> addCoonsPatchWithPoints: controlPoints colors: colors [
	self assert: controlPoints size = 12.
	self assert: colors size = 4.
	^ self addPatch: (CoonsPaintPatch controlPoints: controlPoints colors: colors)
]

{ #category : #adding }
MeshGradientPaint >> addPatch: patch [
	patches add: patch
]

{ #category : #adding }
MeshGradientPaint >> addTriangleWithPoints: controlPoints colors: colors [
	self assert: controlPoints size = 3.
	self assert: colors size = 3.
	^ self addPatch: (TrianglePaintPatch controlPoints: controlPoints colors: colors)
]

{ #category : #converting }
MeshGradientPaint >> asAthensPaintOn: aCanvas [
	^ aCanvas surface
		createMeshGradientWithPatches: patches
]

{ #category : #adding }
MeshGradientPaint >> buildPatchWith: aBlock [
	^ self addPatch: (GenericPaintPatch new buildBlock: aBlock)
]

{ #category : #initialization }
MeshGradientPaint >> initialize [
	super initialize.
	patches := OrderedCollection new
]

{ #category : #accessing }
MeshGradientPaint >> patches [
	^ patches
]

{ #category : #accessing }
MeshGradientPaint >> patches: aListOfPatches [
	patches := aListOfPatches copy
]
