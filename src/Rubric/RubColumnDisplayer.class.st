Class {
	#name : #RubColumnDisplayer,
	#superclass : #RubScrolledTextExtraSurface,
	#instVars : [
		'tabLineDashesColors',
		'tabLineColor',
		'tabLineDashes',
		'tabLineWidth'
	],
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #querying }
RubColumnDisplayer class >> key [
	^ #column
]

{ #category : #drawing }
RubColumnDisplayer >> drawOn: aCanvas [
	| cwidth col dsh lw cw dc nb bnds gap |
	super drawOn: aCanvas.
	bnds := self bounds.
	cwidth := self width.
	cw := self tabWidth.
	gap := self offset x \\ cw .
	col := cw + self margins left.
	dsh := self tabLineDashes.
	dc := {(self tabLineColor). (Color transparent)}.
	lw := self tabLineWidth.
	nb := cwidth // cw.
	nb timesRepeat: [ 
			| b |
			b :=  (bnds topLeft + (col @ 0) ) corner: (bnds bottomLeft + ((col + lw) @ 0) ).
			b := b translateBy: (((cw - gap) - cw)@ 0).
			aCanvas
				frameRectangle: b
				width: 1
				colors: dc
				dashes: dsh.
			col := col + cw ]
]

{ #category : #drawing }
RubColumnDisplayer >> drawOnAthensCanvas: anAthensCanvas [
	| cwidth col dsh lw cw dc nb bnds gap |
	super drawOnAthensCanvas: anAthensCanvas.
	bnds := self bounds.
	cwidth := self width.
	cw := self tabWidth.
	gap := self offset x \\ cw.
	col := cw + self margins left.
	dsh := self tabLineDashes.
	dc := {(self tabLineColor). (Color transparent)}.
	lw := self tabLineWidth.
	nb := cwidth // cw.
	(anAthensCanvas setStrokePaint: self tabLineColor)
		dashes: self tabLineDashes offset: 0;
		width: 1.
	nb
		timesRepeat: [ 
			| b |
			b := bnds topLeft + (col @ 0) corner: bnds bottomLeft + ((col + lw) @ 0).
			b := b translateBy: (cw - gap - cw) @ 0.
			anAthensCanvas setShape: b.
			anAthensCanvas draw.
			col := col + cw ]
]

{ #category : #accessing }
RubColumnDisplayer >> font [
	| textStyle |
	textStyle := self textStyle.
	^ textStyle fontAt: textStyle defaultFontIndex
]

{ #category : #accessing }
RubColumnDisplayer >> tabLineColor [
	^ tabLineColor ifNil: [ tabLineColor := Color lightGray twiceLighter] 
]

{ #category : #accessing }
RubColumnDisplayer >> tabLineColor: aColor [
	tabLineColor := aColor
]

{ #category : #accessing }
RubColumnDisplayer >> tabLineDashes [
	^ tabLineDashes ifNil: [tabLineDashes := {self tabLineWidth. self tabLineWidth*3}].

]

{ #category : #accessing }
RubColumnDisplayer >> tabLineDashes: anArray [
	 tabLineDashes := anArray

]

{ #category : #accessing }
RubColumnDisplayer >> tabLineDashesColors [ 
	^ tabLineDashesColors ifNil: [tabLineDashesColors := {(self tabLineColor). (Color transparent)}].

]

{ #category : #accessing }
RubColumnDisplayer >> tabLineWidth [
	^ tabLineWidth ifNil: [ tabLineWidth := 1 ]
]

{ #category : #accessing }
RubColumnDisplayer >> tabLineWidth: anInteger [
	tabLineWidth := anInteger
]

{ #category : #accessing }
RubColumnDisplayer >> tabWidth [
	^ self paragraph tabWidth
]

{ #category : #accessing }
RubColumnDisplayer >> tabWidth: anInteger [
	self paragraph tabWidth: anInteger
]
