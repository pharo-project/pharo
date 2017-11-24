"
The tiger shape (see VGTigerDemo runDemo)
"
Class {
	#name : #AthensTigerShape,
	#superclass : #Object,
	#instVars : [
		'paint',
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
	#category : #'Athens-Examples'
}

{ #category : #'as yet unclassified' }
AthensTigerShape >> addFill [
	fill := true.
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> addStroke [
	stroke := true.
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> capStyle: cap [
	capStyle := cap.
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> fillPaint: aColor [ 
	fillPaint := aColor
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> fillRule [
	^ fillRule
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> fillRule: rule [ 
	fillRule := rule.
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> initialize [ 
	fill := stroke := false.
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> joinStyle: join [
	joinStyle := join.
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> miterLimit: lim [
	miterLimit := lim.
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> prepareFor: surface [
	fill ifTrue: [
		fillPaint := surface createSolidColorPaint: fillPaint.
	].

	stroke ifTrue: [
		strokePaint := 
			surface createStrokePaintFor: (surface createSolidColorPaint: strokePaint).
	].

	
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> renderOn: can [

	fill ifTrue: [ 
		can setPaint: fillPaint.
		can drawShape: path.	
	].

	stroke ifTrue: [
		can setStrokePaint: strokePaint.	
		can drawShape: path.	
	]
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> setPath: anAthensCairoPath [
		path := anAthensCairoPath
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> strokePaint: aColor [ 
	strokePaint := aColor
]

{ #category : #'as yet unclassified' }
AthensTigerShape >> strokeWidth: w [
	strokeWidth := w
]
