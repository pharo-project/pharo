"
SUnit tests for class Point
"
Class {
	#name : #PointTest,
	#superclass : #ClassTestCase,
	#category : #'Graphics-Tests-Primitives'
}

{ #category : #'tests - arithmetic' }
PointTest >> testAbs [

	self assert: (0 @ 0) abs = (0 @ 0).
	self assert: (1 @ 1) abs = (1 @ 1).
	self assert: (-1 @ -1) abs = (1 @ 1).
	self assert: (-1 @ 1) abs = (1 @ 1).
	self deny: (1 @ -1) abs = (1 @ -1).
	self deny: (-1 @ -1) abs = (-1 @ -1).
	self deny: (-1 @ 1) abs = (-1 @ 1).
	self deny: (1 @ -1) abs = (1 @ -1).
]

{ #category : #'tests - converting' }
PointTest >> testAsFloatPoint [
	|testPoint|
	self assert: (1 @ 1) asFloatPoint = (1.0 @ 1.0).
	self assert: (1.2 @ 1) asFloatPoint = (1.2 @ 1.0).
	self assert: (1 @ 1.2) asFloatPoint = (1.0 @ 1.2).
	self assert: (1.2 @ 1.2) asFloatPoint = (1.2 @ 1.2).
	self assert: (1.000000000000000001 @ 1.9999999999) asFloatPoint = (1.000000000000000001 @ 1.9999999999).
	
	self assert: (-1 @ -1) asFloatPoint = (-1.0 @ -1.0).
	self assert: (-1.2 @ -1) asFloatPoint = (-1.2 @ -1.0).
	self assert: (-1 @ -1.2) asFloatPoint = (-1.0 @ -1.2).
	self assert: (-1.2 @ -1.2) asFloatPoint = (-1.2 @ -1.2).
	self assert: (-1.000000000000000001 @ -1.9999999999) asFloatPoint = (-1.000000000000000001 @ -1.9999999999).
	
	self deny: (1.2 @ 1) asFloatPoint = (1 @ 1).
	self deny: (1 @ 1.2) asFloatPoint = (1 @ 1).
	self deny: (1.2 @ 1.2) asFloatPoint = (1 @ 1).
	
	self deny: (-1.2 @ -1) asFloatPoint = (-1 @ -1).
	self deny: (-1 @ -1.2) asFloatPoint = (-1 @ -1).
	self deny: (-1.2 @ -1.2) asFloatPoint = (-1 @ -1).
	
	"test that we don't create a new instance if point is already a float point"
	testPoint := 1.0 @ 1.0.
	self assert: testPoint == testPoint asFloatPoint.	
	"test that we actually create a new instance if point is not a float one"
	testPoint := 1 @ 1.
	self deny: testPoint == testPoint asFloatPoint
]

{ #category : #'tests - converting' }
PointTest >> testAsIntegerPoint [
	|testPoint|
	self assert: (1 @ 1) asIntegerPoint = (1 @ 1).
	self assert: (1.2 @ 1) asIntegerPoint = (1 @ 1).
	self assert: (1 @ 1.2) asIntegerPoint = (1 @ 1).
	self assert: (1.2 @ 1.2) asIntegerPoint = (1 @ 1).
	self assert: (1.000000000000000001 @ 1.9999999999) asIntegerPoint = (1 @ 1).
	
	self assert: (-1 @ -1) asIntegerPoint = (-1 @ -1).
	self assert: (-1.2 @ -1) asIntegerPoint = (-1 @ -1).
	self assert: (-1 @ -1.2) asIntegerPoint = (-1 @ -1).
	self assert: (-1.2 @ -1.2) asIntegerPoint = (-1 @ -1).
	self assert: (-1.000000000000000001 @ -1.9999999999) asIntegerPoint = (-1 @ -1).
	
	self deny: (1.2 @ 1) asIntegerPoint = (1.2 @ 1).
	self deny: (1 @ 1.2) asIntegerPoint = (1 @ 1.2).
	self deny: (1.2 @ 1.2) asIntegerPoint = (1.2 @ 1.2).
	
	self deny: (-1.2 @ -1) asIntegerPoint = (-1.2 @ -1).
	self deny: (-1 @ -1.2) asIntegerPoint = (-1 @ -1.2).
	self deny: (-1.2 @ -1.2) asIntegerPoint = (-1.2 @ -1.2).
	
	"test that we don't create a new instance if point is already a n integer point"
	testPoint := 1 @ 1.
	self assert: testPoint == testPoint asIntegerPoint.
	"test that we actually create a new instance if point is not an integer one"
	testPoint := 1.0 @ 1.0.
	self deny: testPoint == testPoint asIntegerPoint
]

{ #category : #'tests - converting' }
PointTest >> testAsPoint [

	self deny: 1 isPoint.
	self deny: 1.2 isPoint.
	self assert: 1 asPoint isPoint.
	self assert: -1 asPoint isPoint.
	self assert: 1.2 asPoint isPoint.
	self assert: -1.2 asPoint isPoint.
	self assert: 1 asPoint = (1 @ 1).
	self assert: -1 asPoint = (-1 @ -1).
	self assert: 1.2 asPoint = (1.2 @ 1.2).
	self assert: -1.2 asPoint = (-1.2 @ -1.2).
	self assert: (1 @ 1) asPoint = (1 @ 1).
	self assert: (-1 @ -1) asPoint = (-1 @ -1).
	self assert: (1.2 @ 1.2) asPoint = (1.2 @ 1.2).
	self assert: (-1.2 @ -1.2) asPoint = (-1.2 @ -1.2).
]

{ #category : #'tests - testing' }
PointTest >> testBasicFourDirections [
	"fourDirections returns the four rotation of the receiver in counter clockwise order with the receiver appearing last."
	
	| samples results rejects |
	self assert:  (0@0) fourDirections = {(0@0). (0@0). (0@0). (0@0)}.
	self assert: (0@0) eightNeighbors =  {(1@0). (1@1). (0@1). (-1@1). (-1@0). (-1@ -1). (0@ -1). (1@ -1)}.

	samples :=  {(1@0). (1@1). (0@1). (-1@1). (-1@0). (-1@ -1). (0@ -1). (1@ -1)}.
	results :=  { {0@ -1 . -1@0 . 0@1 . 1@0} 
 				. {1@ -1 . -1@ -1 . -1@1 . 1@1}  
 				. {1@0 . 0@ -1 . -1@0 . 0@1}  
 				. {1@1 . 1@ -1 . -1@ -1 . -1@1}  
 				. {0@1 . 1@0 . 0@ -1 . -1@0}  
 				. {-1@1 . 1@1 . 1@ -1 . -1@ -1}  
 				. {-1@0 . 0@1 . 1@0 . 0@ -1}  
 				. {-1@ -1 . -1@1 . 1@1 . 1@ -1} } .

	rejects := (1 to: samples size ) reject: [ :each |
											 (samples at: each) fourDirections = (results at: each) ].
	self assert: ( rejects isEmpty).

]

{ #category : #'tests - testing' }
PointTest >> testBearingToPoint [
	self assert: (0 @ 0 bearingToPoint: 0 @ 0) = 0.
	self assert: (0 @ 0 bearingToPoint: 0 @ -1) = 0.
	self assert: (0 @ 0 bearingToPoint: 1 @ 0) = 90.
	self assert: (0 @ 0 bearingToPoint: 0 @ 1) = 180.
	self assert: (0 @ 0 bearingToPoint: -1 @ 0) = 270.
	self assert: (0 @ 0 bearingToPoint: 1 @ 1) = 135.
	self assert: (0 @ 0 bearingToPoint: 0.01 @ 0) = 90.
	self assert: (0 @ 0 bearingToPoint: -2 @ -3) = 326.
	self assert: (0 @ 0 bearingToPoint: -0 @ 0) = 0.
	self assert: (-2 @ -3 bearingToPoint: 0 @ 0) = 146
]

{ #category : #'tests - testing' }
PointTest >> testCloseTo [

	| x y |
	x := 0.00001000001.
	y := 0.0000100000001.

	self assert: (x closeTo: 0.00001).
	self assert: (y closeTo: 0.00001).
	
	self assert: (x@y closeTo: (0.00001@0.00001))
]

{ #category : #'tests - point functions' }
PointTest >> testCrossProduct [

	self assert: (0 @ 0 crossProduct: 0 @ 0) = 0.
	self assert: (1 @ 0 crossProduct: 1 @ 1) = 1.
	self assert: (3 @ 2 crossProduct: 6 @ 4) = 0.
	self assert: (1 @ 2 crossProduct: 3 @ 4) = -2.
]

{ #category : #'tests - point functions' }
PointTest >> testDist [

	self assert: (0 @ 0 dist: 0 @ 0) = 0.
	self assert: (0 @ 0 dist: 0 @ 5) = 5.
	self assert: (5 @ 0 dist: 0 @ 0) = 5.
	self assert: (3 @ 0 dist: 0 @ 4) = 5.
	self assert: (5 @ 0 dist: 2 @ 4) = 5.
	self deny: (0 @ 0 dist: 0 @ 0) = 1.
]

{ #category : #'tests - point functions' }
PointTest >> testDistanceTo [

	self assert: (0 @ 0 distanceTo: 0 @ 0) = 0.
	self assert: (0 @ 0 distanceTo: 0 @ 5) = 5.
	self assert: (5 @ 0 distanceTo: 0 @ 0) = 5.
	self assert: (3 @ 0 distanceTo: 0 @ 4) = 5.
	self assert: (5 @ 0 distanceTo: 2 @ 4) = 5.
	self deny: (0 @ 0 distanceTo: 0 @ 0) = 1.
]

{ #category : #'tests - point functions' }
PointTest >> testEightNeighbors [

	| x y |
	x := Point new.
 	x := (1 @ 1).
	y := x eightNeighbors.
	self assert: y = { (2 @ 1) . (2 @ 2) . (1 @ 2) . (0 @ 2) . (0 @ 1) . (0 @ 0) . (1 @ 0) . (2 @ 0)}.
	x := Point new.
	x := -2 @ -6.
	y := x eightNeighbors.
	self assert: y = { (-1 @ -6) . (-1 @ -5) . (-2 @ -5) . (-3 @ -5) . (-3 @ -6) . (-3 @ -7) . (-2 @ -7) . (-1 @ -7)}.

]

{ #category : #'tests - point functions' }
PointTest >> testFlipByCenterAt [

	| center a direction |
	
	center := 4 @ 5.
	
	a := 0 @ 0.
	direction := #vertical.
	self assert: (a flipBy: direction centerAt: center) = (0 @ 10).
	direction := #horizontal.
	self assert: (a flipBy: direction centerAt: center) = (8 @ 0).
	direction := #diagonal.
	self should: [ a flipBy: direction centerAt: center ] raise: Error description: 'unrecognizable direction'.
	
	a := 1 @ 2.
	direction := #vertical.
	self deny: (a flipBy: direction centerAt: center) = (0 @ 10).
	direction := #horizontal.
	self deny: (a flipBy: direction centerAt: center) = (8 @ 0).
	direction := #diagonal.
	self should: [ a flipBy: direction centerAt: center ] raise: Error description: 'unrecognizable direction'
]

{ #category : #'tests - point functions' }
PointTest >> testInsideTriangleWithWith [
	self assert:   ( (2 @ 3) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self deny:    ( (3 @ 1) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self deny:    ( (3 @ 15) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self deny:    ( (0 @ 6) insideTriangle:  (1 @ 1 ) with: (1 @ 7) with: (3 @ 3)).
	self assert:   ( (-2 @ -3) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).
	self deny:    ( (-3 @ -1) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).
	self deny:    ( (-3 @ -15) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).
	self deny:    ( (0 @ -6) insideTriangle:  (-1 @ -1 ) with: (-1 @ -7) with: (-3 @ -3)).
]

{ #category : #'tests - truncation and roundoff' }
PointTest >> testIsFloatPoint [

	self deny: (1 @ 1) isFloatPoint.
	self deny: (-1 @ 1) isFloatPoint.
	self deny: (1 @ -1) isFloatPoint.
	self deny: (-1 @ -1) isFloatPoint.
	
	self deny: (1 @ 1.2) isFloatPoint.
	self deny: (1.2 @ 1) isFloatPoint.
	self assert: (1.2 @ 1.2) isFloatPoint.
	
	self deny: (-1 @ -1.2) isFloatPoint.
	self deny: (-1.2 @ -1) isFloatPoint.
	self assert: (-1.2 @ -1.2) isFloatPoint.
	
	self deny: (0 @ 0) isFloatPoint.
	self deny: (0.0 @ 0) isFloatPoint.
	self deny: (0 @ 0.0) isFloatPoint.
	self assert: (0.0 @ 0.0) isFloatPoint.
	
]

{ #category : #'tests - truncation and roundoff' }
PointTest >> testIsIntegerPoint [

	self assert: (1 @ 1) isIntegerPoint.
	self assert: (-1 @ 1) isIntegerPoint.
	self assert: (1 @ -1) isIntegerPoint.
	self assert: (-1 @ -1) isIntegerPoint.
	
	self deny: (1 @ 1.2) isIntegerPoint.
	self deny: (1.2 @ 1) isIntegerPoint.
	self deny: (1.2 @ 1.2) isIntegerPoint.
	
	self deny: (-1 @ -1.2) isIntegerPoint.
	self deny: (-1.2 @ -1) isIntegerPoint.
	self deny: (-1.2 @ -1.2) isIntegerPoint.
	
]

{ #category : #'tests - converting' }
PointTest >> testIsPoint [

	self deny: nil isPoint.
	self deny: 'a' isPoint.
	self deny: 1 isPoint. 
	self deny: -1 isPoint. 
	'Test on a random class'.
	self deny: KernelClassesFilter new isPoint.
	self deny: Object new isPoint.
	self assert: Point new isPoint.
	self assert: (0 @ 0) isPoint.
	self assert: (-1 @ -1) isPoint.
	self assert: (1.2 @ 1.2) isPoint.
	self assert: (-1.2 @ -1.2) isPoint.
]

{ #category : #'tests - testing' }
PointTest >> testIsZero [

	self assert: (0@0) isZero.	
	self deny:  (0@1) isZero.
	self deny:  (1@0) isZero.
	self deny:  (1@1) isZero.
]

{ #category : #'tests - testing' }
PointTest >> testLeftRotated [

	self assert: (10 @ 20) leftRotated = (20@ -10)
]

{ #category : #'tests - transforming' }
PointTest >> testNegated [

	self assert: (0 @ 0) negated = (0 @ 0).
	self deny: (0 @ 1) negated = (0 @ 0).
	self deny: (1 @ 0) negated = (0 @ 0).
	self deny: (1 @ 1) negated = (0 @ 0).
	self assert: (1 @ 1) negated = (-1 @ -1).
	self deny: (-1 @ -1) negated = (-1 @ -1).
	self deny: (1 @ -1) negated = (-1 @ -1).
	self deny: (-1 @ 1) negated = (-1 @ -1).
	self assert: (-1 @ -1) negated = (1 @ 1).
	self assert: (1 @ -1) negated = (-1 @ 1).
	self assert: (-1 @ 1) negated = (1 @ -1)
]

{ #category : #'tests - testing' }
PointTest >> testNormalized [
	| pt |
	pt:= Point x:2 y:2.
	
	self assert: pt normalized =  ((2/(8 sqrt))@(2/(8 sqrt))).
]

{ #category : #'tests - arithmetic' }
PointTest >> testReciprocal [

	self assert: (1 @ 1) reciprocal = (1 @ 1).
	self deny: (2 @ 2) reciprocal = (2 @ 2).
	self assert: (1 @ 5) reciprocal = (1 @ (1 / 5)).
	self should: [ (0 @ 0) reciprocal ] raise: ZeroDivide.
	self should: [ (1 @ 0) reciprocal ] raise: ZeroDivide.
	self should: [ (0 @ 1) reciprocal ] raise: ZeroDivide
]

{ #category : #'tests - testing' }
PointTest >> testRightRotated [

	self assert: (10 @ 20) rightRotated = (-20@10)
]

{ #category : #'tests - testing' }
PointTest >> testScaleTo [
	self assert: (200@200 scaleTo: 400@400) equals:(2.0@2.0).
	self assert: (40@40 scaleTo: 400@400) equals: (10.0@10.0)
	
]

{ #category : #'tests - testing' }
PointTest >> testSideOf [
	| p1 p2 p3 p4 p5 p6 o|
	
	p1:= 1@1.
	p2:= -1@(-1).
	p3:= -1@0.
	p4:= 0@1.
	p5:= 1@0.
	p6:=0@(-1).
	o := 0@0.
	
	self assert: (p1 sideOf:o) = #center.
	self assert: (p1 sideOf:p2) = #center.
	
	self assert: (p1 sideOf:p3) = #left.
	self assert:(p1 sideOf:p4) = #left.
	
	self assert: (p1 sideOf:p5) = #right.
	self assert:(p1 sideOf:p6) = #right.
]

{ #category : #'tests - testing' }
PointTest >> testSign [

	self assert: (-3 @ -2) sign = ( -1@ -1).
	self assert: (-3 @ 2) sign = ( -1@ 1).
	self assert: (3 @ -2) sign = ( 1@ -1).
	self assert: (3 @ 2) sign = ( 1@ 1).
]

{ #category : #'tests - testing' }
PointTest >> testTheta [
      | result dir tan x y |
	self assert: ((0 @ 1) theta - 90.0 degreesToRadians) abs < 1e-15.
	self assert: ((0 @ -1) theta - 270.0 degreesToRadians) abs < 1e-15.
	" See code of old and new theta"
	x := 1.0 . y := -1.0.
	tan := y  / x .
	dir := tan arcTan.
      result :=  360.0 degreesToRadians + dir.
      self assert: ((x @ y) theta - result) abs < 1e-15.
	x := -1.0. "Don't reuse old results whenyou want numeric precision!"
	tan := y  / x .
	dir := tan arcTan.
      result :=  180.0 degreesToRadians + dir.
      self assert: ((x @ y) theta - result) abs < 1e-15.

	
]

{ #category : #'tests - geometry' }
PointTest >> testToIntersectsTo [

	self assert: ( (1 @ 1) to: ( 1 @ 6) intersects: (1 @ 1) to: (2 @ 9) ).
	self assert: ( (1 @ 1) to: ( 1 @ 6) intersects: (1 @ 2) to: (1 @6) ).
	self assert: ( (1 @ 1) to: ( 2 @ 6) intersects: (8 @ 6) to: (1 @ 1) ).
	self assert: ( (5 @ 1) to: ( 1 @ 6) intersects: (1 @ 6) to: (9 @ 1) ).
	self deny:  ( (1 @ 1) to: ( 1 @ 6) intersects: (2 @ 1) to: (2 @ 6) ). 
	self deny:  ( (1 @ 1) to: ( 1 @ 6) intersects: (0 @7) to: (5 @ 7) ). 
	self assert: ( (1 @ 1) to: ( 1 @ 6) intersects: (0 @ 2) to: (2 @ 2) ). 
	
	self assert: ( (-1 @ -1) to: ( -1 @ -6) intersects: (-1 @ -1) to: (-2 @ -9) ).
	self assert: ( (-1 @ -1) to: ( -1 @ -6) intersects: (-1 @ -2) to: (-1 @ -6) ).
	self assert: ( (-1 @ -1) to: ( -2 @ -6) intersects: (-8 @ -6) to: (-1 @ -1) ).
	self assert: ( (-5 @ -1) to: ( -1 @ -6) intersects: (-1 @ -6) to: (-9 @ -1) ).
	self deny:  ( (-1 @ -1) to: (-1 @ -6) intersects: (-2 @ -1) to: (-2 @ -6) ). 
	self deny:  ( (-1 @ -1) to: ( -1 @ -6) intersects: (-0 @ -7) to: (-5 @ -7) ). 
	self assert: ( (-1 @ -1) to: ( -1 @ -6) intersects: (-0 @ -2) to: (-2 @ -2) ). 
	
	"point line( 1@1 to: 1@1 ) not allowed "
	self deny: ( (1 @ 1) to: ( 1 @ 1) intersects: (1@ 0) to: (1 @ 2) )."should be true if point line alowed" 
	self assert: ( (1 @ 1) to: ( 1 @ 1) intersects: (1 @ 1) to: (1 @ 9) ). 

]
