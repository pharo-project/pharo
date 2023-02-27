"
I'm a visual part of the AthensTiger.
"
Class {
	#name : #AthensTigerPart,
	#superclass : #Object,
	#instVars : [
		'path',
		'fillRule',
		'fill',
		'capStyle',
		'joinStyle',
		'miterLimit',
		'strokeWidth',
		'strokePaint',
		'fillPaint',
		'stroke'
	],
	#category : #'Athens-Examples-Utilities'
}

{ #category : #accessing }
AthensTigerPart >> addFill [
	fill := true
]

{ #category : #accessing }
AthensTigerPart >> addStroke [
	stroke := true
]

{ #category : #accessing }
AthensTigerPart >> capStyle [
	^capStyle
]

{ #category : #accessing }
AthensTigerPart >> capStyle: cap [
	capStyle := cap
]

{ #category : #accessing }
AthensTigerPart >> fillPaint: aColor [
	fillPaint := aColor
]

{ #category : #accessing }
AthensTigerPart >> fillRule [
	^ fillRule
]

{ #category : #accessing }
AthensTigerPart >> fillRule: rule [
	fillRule := rule
]

{ #category : #initialization }
AthensTigerPart >> initialize [
	fill := stroke := false
]

{ #category : #accessing }
AthensTigerPart >> joinStyle [
	^joinStyle
]

{ #category : #accessing }
AthensTigerPart >> joinStyle: join [
	joinStyle := join
]

{ #category : #accessing }
AthensTigerPart >> miterLimit [
	^miterLimit
]

{ #category : #accessing }
AthensTigerPart >> miterLimit: lim [
	miterLimit := lim
]

{ #category : #rendering }
AthensTigerPart >> prepareFor: surface [
	fill ifTrue: [
		fillPaint := surface createSolidColorPaint: fillPaint.
	].

	stroke ifTrue: [
		strokePaint :=
			surface createStrokePaintFor: (surface createSolidColorPaint: strokePaint).
	]
]

{ #category : #rendering }
AthensTigerPart >> renderOn: can [

	fill ifTrue: [
		can setPaint: fillPaint.
		can drawShape: path.
	].

	stroke ifTrue: [
		can setStrokePaint: strokePaint.
		can drawShape: path.
	]
]

{ #category : #accessing }
AthensTigerPart >> setPath: anAthensCairoPath [
		path := anAthensCairoPath
]

{ #category : #accessing }
AthensTigerPart >> strokePaint: aColor [
	strokePaint := aColor
]

{ #category : #accessing }
AthensTigerPart >> strokeWidth [
	^strokeWidth
]

{ #category : #accessing }
AthensTigerPart >> strokeWidth: w [
	strokeWidth := w
]
