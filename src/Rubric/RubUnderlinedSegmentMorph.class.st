Class {
	#name : #RubUnderlinedSegmentMorph,
	#superclass : #RubTextSegmentMorph,
	#instVars : [
		'underlineColor',
		'straight',
		'underlineShape'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #drawing }
RubUnderlinedSegmentMorph >> computeVertices [
	| lpos rpos fln lln |
	super computeVertices.
	fln := self firstLineIndex.
	lln := self lastLineIndex.
	underlineShape := OrderedCollection new.
	fln to: lln do: [ :idx | 
		| line cidx |
		line := self lines at: idx.
		cidx := idx = fln
			ifTrue: [ firstIndex ]
			ifFalse: [ line first ].
		lpos := (self characterBlockForIndex: cidx) bottomLeft.
		cidx := idx = lln
			ifTrue: [ lastIndex ]
			ifFalse: [ line last ].
		rpos := (self characterBlockForIndex: cidx) bottomLeft.
		underlineShape add: (self underlineShapeFromPosition: lpos toPosition: rpos) ]
]

{ #category : #initialization }
RubUnderlinedSegmentMorph >> defaultBorderColor [
	^ super defaultColor alpha: 0.5.

]

{ #category : #initialization }
RubUnderlinedSegmentMorph >> defaultColor [
	^ Color white alpha: 0.05

]

{ #category : #initialization }
RubUnderlinedSegmentMorph >> defaultUnderlineColor [
	^ super defaultColor

]

{ #category : #drawing }
RubUnderlinedSegmentMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	self drawUnderlineOn: aCanvas
]

{ #category : #drawing }
RubUnderlinedSegmentMorph >> drawOnAthensCanvas: anAthensCanvas [
	super drawOnAthensCanvas: anAthensCanvas.
	self drawUnderlineOnAthensCanvas: anAthensCanvas
]

{ #category : #drawing }
RubUnderlinedSegmentMorph >> drawUnderlineOn: aCanvas [
	underlineShape
		do: [ :s | 
			(1 to: s size - 1 by:
				(self straight
					ifTrue: [ 2 ]
					ifFalse: [ 1 ]))
				do: [ :p | 
					| s1 s2 |
					s1 := s at: p.
					s2 := s at: p + 1.
					aCanvas
						line: s1
						to: s2
						width: 2
						color: self underlineColor ] ]
]

{ #category : #drawing }
RubUnderlinedSegmentMorph >> drawUnderlineOnAthensCanvas: aCanvas [
	underlineShape
		do: [ :s | 
			| path |
			(1 to: s size - 1 by:
				(self straight
					ifTrue: [ 2 ]
					ifFalse: [ 1 ]))
				do: [ :p | 
					| s1 s2 |
					s1 := s at: p.
					s2 := s at: p + 1.
					path := aCanvas
						createPath: [ :builder | 
							builder absolute.
							builder moveTo: s1.
							builder lineTo: s2 ].
					(aCanvas setStrokePaint: self underlineColor) width: 1.
					aCanvas drawShape: path ] ]
]

{ #category : #accessing }
RubUnderlinedSegmentMorph >> straight [
	^ straight ifNil: [ straight := true ]
	
]

{ #category : #accessing }
RubUnderlinedSegmentMorph >> straight: aBoolean [
	straight := aBoolean.
	self computeVertices 
	
]

{ #category : #drawing }
RubUnderlinedSegmentMorph >> tabLineDashes [
	^ { 1. 1} 
]

{ #category : #accessing }
RubUnderlinedSegmentMorph >> underlineColor [
	^ underlineColor ifNil: [ underlineColor := self defaultUnderlineColor ]
]

{ #category : #accessing }
RubUnderlinedSegmentMorph >> underlineColor: aColor [
	underlineColor := aColor
]

{ #category : #drawing }
RubUnderlinedSegmentMorph >> underlineShapeFromPosition: firstPos toPosition: lastPos [
	| shape ygap pos |
	shape := OrderedCollection new.
	ygap := self straight
		ifTrue: [ 0 ]
		ifFalse: [ 1 ].
	shape add: (pos := firstPos x @ (firstPos y + (ygap // 2))).
	[ pos x < (lastPos x min: self right) ]
		whileTrue: [ 
			ygap := ygap negated.
			pos := (pos x + 2 min: self right) @ (pos y + ygap).
			shape add: pos.
			pos := pos + (1 @ 0) ].
	^ shape
]
