"
SUnit tests for class LayoutFrame
"
Class {
	#name : #LayoutFrameTest,
	#superclass : #TestCase,
	#category : #'Morphic-Tests-Layouts'
}

{ #category : #'tests - conversion' }
LayoutFrameTest >> testAsLayoutFrame [
	| frame |
	
	"do not use this method. Do not create an explicit array to create a layout
	only use it, if you are given an array from somewhere else"
	frame := #(0.66 0.5 1 0.77 1 2 3 25) asLayoutFrame.
	self assert: frame leftFraction equals: 0.66.
	self assert: frame topFraction equals: 0.5.
	self assert: frame leftOffset equals: 1.
	self assert: frame topOffset equals: 2.
	
	self assert: frame rightFraction equals: 1.
	self assert: frame bottomFraction equals: 0.77.
	self assert: frame rightOffset equals: 3.
	self assert: frame bottomOffset equals: 25.
]

{ #category : #'tests - conversion' }
LayoutFrameTest >> testIdentity [
	self assert: LayoutFrame new equals: LayoutFrame identity
]

{ #category : #tests }
LayoutFrameTest >> testInset [
	| lf rectangle |
	lf := LayoutFrame new
			leftOffset: 10;
			topOffset: 10;
			rightOffset: -10;
			bottomOffset: -10;
			yourself.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (60@20 corner: 140@60) = rectangle
]

{ #category : #tests }
LayoutFrameTest >> testLeftTopAligned [
	| lf rectangle |
	lf := LayoutFrame new
		leftOffset: 10;
		topOffset: 10;
		rightFraction: 0 offset: 60;
		bottomFraction: 0 offset: 25;
		yourself.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (60@20 corner: 110@35) = rectangle
]

{ #category : #tests }
LayoutFrameTest >> testRightBottomQuadrant [
	| lf rectangle |
	lf := LayoutFrame new
			leftFraction: 1/2 offset: 1;
			topFraction: 1/2 offset: 1;
			rightFraction: 1 offset: -2;
			bottomFraction: 1 offset: -2;
			yourself.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (101@41 corner: 148@68) = rectangle
]

{ #category : #tests }
LayoutFrameTest >> testSpaceFill [
	| lf rectangle |
	lf := LayoutFrame identity.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (50@10 corner: 150@70) = rectangle
]

{ #category : #tests }
LayoutFrameTest >> testTransform [
	"the transform: operation is transforming a layoutFrame relatively to the reference rectangle: basically you transform to obtain a subrectangle area inside the reference rectangle"

	| lf rectangle |
	lf := LayoutFrame new
			leftOffset: 10;
			topOffset: 10;
			rightOffset: -10;
			bottomOffset: -10;
			yourself.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (60@20 corner: 140@60) = rectangle
]

{ #category : #tests }
LayoutFrameTest >> testTransformReturnASubArea [
	"the transform: operation is transforming a layoutFrame relatively to the reference rectangle: basically you transform to obtain a subrectangle area inside the reference rectangle"

	| lf rectangle refRectangle |
	lf := LayoutFrame new
			leftOffset: 10;
			topOffset: 10;
			rightOffset: -10;
			bottomOffset: -10;
			yourself.
	refRectangle := 0@0 extent: 100@200.
	rectangle := lf transform: refRectangle.
	self assert: rectangle width = 80.
	self assert: rectangle height = 180.
]

{ #category : #tests }
LayoutFrameTest >> testTransformReturnEmptyRectangleWhenNotPossibleToPlace [
	"the transform: operation is transforming a layoutFrame relatively to the reference rectangle: basically you transform to obtain a subrectangle area inside the reference rectangle"

	| lf rectangle refRectangle |
	lf := LayoutFrame new
			leftOffset: 10;
			topOffset: 10;
			rightOffset: -10;
			bottomOffset: -10;
			yourself.
	refRectangle := 0@0 extent: 10@10.
	rectangle := lf transform: refRectangle.
	self assert: rectangle width = 0.
	self assert: rectangle height = 0.
]
