"
This class holds tests for PolygonMorph
"
Class {
	#name : #PolygonMorphTest,
	#superclass : #MorphTest,
	#category : #'Morphic-Tests-Basic'
}

{ #category : #tests }
PolygonMorphTest >> testBoundsBug1035 [
	"This is a non regression test for http://bugs.squeak.org/view.php?id=1035
	PolygonMorph used to position badly when container bounds were growing"

	| submorph aMorph |
	submorph := (PolygonMorph
		vertices: {(0 @ 0) . (100 @ 0) . (0 @ 100)}
		color: Color red
		borderWidth: 0
		borderColor: Color transparent) color: Color red.

	submorph bounds.	"0@0 corner: 100@100"

	aMorph := Morph new
		color: Color blue;
		layoutPolicy: ProportionalLayout new;
		addMorph: submorph fullFrame: (0.1 @ 0.1 corner: 0.9 @ 0.9) asLayoutFrame.

	submorph bounds.	"0@0 corner: 100@100 NOT YET UPDATED"
	aMorph fullBounds.	"0@0 corner: 50@40. CORRECT"
	submorph bounds.	"5@4 corner: 45@36 NOW UPDATED OK"

	aMorph extent: 100 @ 100.
	submorph bounds.	"5@4 corner: 45@36 NOT YET UPDATED"
	aMorph fullBounds.	"-10@-14 corner: 100@100 WRONG"
	submorph bounds.	"-10@-14 corner: 70@66 NOW WRONG POSITION (BUT RIGHT EXTENT)"

	self assert: aMorph fullBounds equals: (0 @ 0 extent: 100 @ 100).
	self assert: submorph bounds equals: (10 @ 10 corner: 90 @ 90)
]
