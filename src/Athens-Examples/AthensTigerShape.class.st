"
The tiger shape (see VGTigerDemo runDemo)
"
Class {
	#name : #AthensTigerShape,
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
AthensTigerShape >> addFill [
	fill := true.
]

{ #category : #accessing }
AthensTigerShape >> addStroke [
	stroke := true.
]

{ #category : #accessing }
AthensTigerShape >> capStyle [
	^capStyle
]

{ #category : #accessing }
AthensTigerShape >> capStyle: cap [
	capStyle := cap.
]

{ #category : #accessing }
AthensTigerShape >> fillPaint: aColor [ 
	fillPaint := aColor
]

{ #category : #accessing }
AthensTigerShape >> fillRule [
	^ fillRule
]

{ #category : #accessing }
AthensTigerShape >> fillRule: rule [ 
	fillRule := rule.
]

{ #category : #initialization }
AthensTigerShape >> initialize [ 
	fill := stroke := false.
]

{ #category : #accessing }
AthensTigerShape >> joinStyle [
	^joinStyle
]

{ #category : #accessing }
AthensTigerShape >> joinStyle: join [
	joinStyle := join.
]

{ #category : #accessing }
AthensTigerShape >> miterLimit [
	^miterLimit
]

{ #category : #accessing }
AthensTigerShape >> miterLimit: lim [
	miterLimit := lim.
]

{ #category : #rendering }
AthensTigerShape >> prepareFor: surface [
	fill ifTrue: [
		fillPaint := surface createSolidColorPaint: fillPaint.
	].

	stroke ifTrue: [
		strokePaint := 
			surface createStrokePaintFor: (surface createSolidColorPaint: strokePaint).
	].

	
]

{ #category : #rendering }
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

{ #category : #accessing }
AthensTigerShape >> setPath: anAthensCairoPath [
		path := anAthensCairoPath
]

{ #category : #accessing }
AthensTigerShape >> strokePaint: aColor [ 
	strokePaint := aColor
]

{ #category : #accessing }
AthensTigerShape >> strokeWidth [
	^strokeWidth
]

{ #category : #accessing }
AthensTigerShape >> strokeWidth: w [
	strokeWidth := w
]
