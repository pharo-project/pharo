"
An AthensTransformTest is a test class for testing the behavior of AthensTransform
"
Class {
	#name : #AthensTransformTest,
	#superclass : #TestCase,
	#category : #'Athens-Core-Tests-Transforms'
}

{ #category : #testing }
AthensTransformTest class >> isAbstract [
	^ self == AthensTransformTest
]

{ #category : #'instance creation' }
AthensTransformTest >> newTransform [
	^ self transformClass new
]

{ #category : #tests }
AthensTransformTest >> testIdentity [
	| transform |
	transform := self newTransform.
	self assert: transform isIdentity.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5
]

{ #category : #tests }
AthensTransformTest >> testInvert [
	| transform |
	transform := self newTransform.
	transform
		translateX: 1 Y: 9;
		rotateByDegrees: 90;
		scaleX: 3 Y: 5;
		invert.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 1 @ 9) closeTo: 0 @ 0.
	self assert: (transform transform: 0@0) closeTo: -3@0.2.
	self assert: (transform transform: 10@3) closeTo: -2 @ -1.8.
	self assert: (transform transform: -3 @ -6) closeTo: -5 @ 0.8.
	self assert: (transform transform: 1 @ -18) closeTo: -9 @ 0.
	self assert: (transform transform: -2 @ -3) closeTo: -4 @ 0.6
]

{ #category : #tests }
AthensTransformTest >> testLoadAffineTransform [
	| transform affineTransform |
	transform := self newTransform.
	affineTransform := AthensAffineTransform new.
	affineTransform
		translateX: 1 Y: 9;
		rotateByDegrees: 90;
		scaleX: 3 Y: 5.
	transform loadAffineTransform: affineTransform.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 1@9.
	self assert: (transform transform: 1@2) closeTo: -9@12.
	self assert: (transform transform: -3 @ -4) closeTo: 21 @ 0.
	self assert: (transform transform: 1 @ -2) closeTo: 11 @ 12.
	self assert: (transform transform: -2 @ -5) closeTo: 26 @ 3
]

{ #category : #tests }
AthensTransformTest >> testLoadIdentity [
	| transform |
	transform := self newTransform.
	transform translateX: 5 Y: 0; rotateByDegrees: 45; scaleX: 5 Y: 42.
	transform loadIdentity.

	self assert: transform isIdentity.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5
]

{ #category : #tests }
AthensTransformTest >> testRestoreAfter [
	| transform |
	transform := self newTransform.
	transform
		translateX: 1 Y: 9;
		rotateByDegrees: 90;
		scaleX: 3 Y: 5.

	transform restoreAfter: [
		transform
			loadIdentity;
			translateBy: 4 @ -3.
		self assert: transform isAffineTransform.
		self assert: (transform transform: 0@0) equals: 4 @ -3.
		self assert: (transform transform: 1@2) equals: 5 @ -1.
		self assert: (transform transform: -3 @ -4) equals: 1 @ -7.
		self assert: (transform transform: 1 @ -2) equals: 5 @ -5.
		self assert: (transform transform: -2 @ -5) equals: 2 @ -8.
	].

	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 1@9.
	self assert: (transform transform: 1@2) closeTo: -9@12.
	self assert: (transform transform: -3 @ -4) closeTo: 21 @ 0.
	self assert: (transform transform: 1 @ -2) closeTo: 11 @ 12.
	self assert: (transform transform: -2 @ -5) closeTo: 26 @ 3
]

{ #category : #tests }
AthensTransformTest >> testRotateByDegrees [
	| transform |
	transform := self newTransform.
	transform rotateByDegrees: 0.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5.

	transform rotateByDegrees: 180.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 0@0.
	self assert: (transform transform: 1@2) closeTo: -1 @ -2.
	self assert: (transform transform: -3 @ -4) closeTo: 3 @ 4.
	self assert: (transform transform: 1 @ -2) closeTo: -1 @ 2.
	self assert: (transform transform: -2 @ -5) closeTo: 2 @ 5.

	transform rotateByDegrees: -90.
	"Rotation: 180 - 90 = 90"
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 0@0.
	self assert: (transform transform: 1@2) closeTo: -2@1.
	self assert: (transform transform: -3 @ -4) closeTo: 4 @ -3.
	self assert: (transform transform: 1 @ -2) closeTo: 2 @ 1.
	self assert: (transform transform: -2 @ -5) closeTo: 5 @ -2
]

{ #category : #tests }
AthensTransformTest >> testRotateByRadians [
	| transform |
	transform := self newTransform.
	transform rotateByRadians: 0.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5.

	transform rotateByRadians: Float pi.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 0@0.
	self assert: (transform transform: 1@2) closeTo: -1 @ -2.
	self assert: (transform transform: -3 @ -4) closeTo: 3 @ 4.
	self assert: (transform transform: 1 @ -2) closeTo: -1 @ 2.
	self assert: (transform transform: -2 @ -5) closeTo: 2 @ 5.

	transform rotateByRadians: Float halfPi negated.
	"Rotation: pi - pi/2 = pi/2"
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 0@0.
	self assert: (transform transform: 1@2) closeTo: -2@1.
	self assert: (transform transform: -3 @ -4) closeTo: 4 @ -3.
	self assert: (transform transform: 1 @ -2) closeTo: 2 @ 1.
	self assert: (transform transform: -2 @ -5) closeTo: 5 @ -2
]

{ #category : #tests }
AthensTransformTest >> testScaleBy [
	| transform |
	transform := self newTransform.
	transform scaleBy: 1.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5.

	transform scaleBy: 3.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 3@6.
	self assert: (transform transform: -3 @ -4) equals: -9 @ -12.
	self assert: (transform transform: 1 @ -2) equals: 3 @ -6.
	self assert: (transform transform: -2 @ -5) equals: -6 	@ -15.

	transform scaleBy: -0.5.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 0@0.
	self assert: (transform transform: 1@2) closeTo: -1.5 @ -3.
	self assert: (transform transform: -3 @ -4) closeTo: 4.5 @ 6.
	self assert: (transform transform: 1 @ -2) closeTo: -1.5 @ 3.
	self assert: (transform transform: -2 @ -5) closeTo: 3 @ 7.5
]

{ #category : #tests }
AthensTransformTest >> testScaleXY [
	| transform |
	transform := self newTransform.
	transform scaleX: 1 Y: 1.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5.

	transform scaleX: 3 Y: -3.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 3@ -6.
	self assert: (transform transform: -3 @ -4) equals: -9 @ 12.
	self assert: (transform transform: 1 @ -2) equals: 3 @ 6.
	self assert: (transform transform: -2 @ -5) equals: -6 	@ 15.

	transform scaleX: -0.5 Y: 0.5.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 0@0.
	self assert: (transform transform: 1@2) closeTo: -1.5 @ -3.
	self assert: (transform transform: -3 @ -4) closeTo: 4.5 @ 6.
	self assert: (transform transform: 1 @ -2) closeTo: -1.5 @ 3.
	self assert: (transform transform: -2 @ -5) closeTo: 3 @ 7.5
]

{ #category : #tests }
AthensTransformTest >> testTransform [
	| transform |
	transform := self newTransform.
	transform
		translateX: 1 Y: 9;
		rotateByDegrees: 90;
		scaleX: 3 Y: 5.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) closeTo: 1@9.
	self assert: (transform transform: 1@2) closeTo: -9@12.
	self assert: (transform transform: -3 @ -4) closeTo: 21 @ 0.
	self assert: (transform transform: 1 @ -2) closeTo: 11 @ 12.
	self assert: (transform transform: -2 @ -5) closeTo: 26 @ 3
]

{ #category : #tests }
AthensTransformTest >> testTransformRectangle [
	| transform |
	transform := self newTransform.
	self assert: transform isAffineTransform.
	self assert: (transform transformRectangle: (0@0 extent: 10@20)) closeTo: (0@0 extent: 10@20).

	transform translateX: 5 Y: 3.
	self assert: (transform transformRectangle: (0@0 extent: 10@20)) closeTo: (5@3 extent: 10@20).

	transform scaleX: 4 Y: 3.
	self assert: (transform transformRectangle: (0@0 extent: 10@20)) closeTo: (5@3 extent: 40@60).

	transform rotateByDegrees: 90.
	self assert: (transform transformRectangle: (0@0 extent: 10@20)) closeTo: (-75@3 extent: 80@30)
]

{ #category : #tests }
AthensTransformTest >> testTransformXY [
	| transform |
	transform := self newTransform.
	transform
		translateX: 1 Y: 9;
		rotateByDegrees: 90;
		scaleX: 3 Y: 5.
	self assert: transform isAffineTransform.
	self assert: (transform transformX: 0 Y: 0) closeTo: 1@9.
	self assert: (transform transformX: 1 Y: 2) closeTo: -9@12.
	self assert: (transform transformX: -3 Y: -4) closeTo: 21 @ 0.
	self assert: (transform transformX: 1 Y: -2) closeTo: 11 @ 12.
	self assert: (transform transformX: -2 Y: -5) closeTo: 26 @ 3
]

{ #category : #tests }
AthensTransformTest >> testTranslate [
	| transform |
	transform := self newTransform.
	transform translateBy: 0 @ 0.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5.

	transform translateBy: 4 @ -3.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 4 @ -3.
	self assert: (transform transform: 1@2) equals: 5 @ -1.
	self assert: (transform transform: -3 @ -4) equals: 1 @ -7.
	self assert: (transform transform: 1 @ -2) equals: 5 @ -5.
	self assert: (transform transform: -2 @ -5) equals: 2 @ -8.

	transform translateBy: -3 @ 5.
	"Translation: (4 @ -3) + (-3 @ 5) = 1@2 "
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 1@2.
	self assert: (transform transform: 1@2) equals: 2@4.
	self assert: (transform transform: -3 @ -4) equals: -2 @ -2.
	self assert: (transform transform: 1 @ -2) equals: 2 @ 0.
	self assert: (transform transform: -2 @ -5) equals: -1 	@ -3
]

{ #category : #tests }
AthensTransformTest >> testTranslateBy [
	| transform |
	transform := self newTransform.
	transform translateBy: 0 @ 0.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5.

	transform translateBy: 4 @ -3.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 4 @ -3.
	self assert: (transform transform: 1@2) equals: 5 @ -1.
	self assert: (transform transform: -3 @ -4) equals: 1 @ -7.
	self assert: (transform transform: 1 @ -2) equals: 5 @ -5.
	self assert: (transform transform: -2 @ -5) equals: 2 @ -8.

	transform translateBy: -3 @ 5.
	"Translation: (4 @ -3) + (-3 @ 5) = 1@2 "
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 1@2.
	self assert: (transform transform: 1@2) equals: 2@4.
	self assert: (transform transform: -3 @ -4) equals: -2 @ -2.
	self assert: (transform transform: 1 @ -2) equals: 2 @ 0.
	self assert: (transform transform: -2 @ -5) equals: -1 	@ -3
]

{ #category : #tests }
AthensTransformTest >> testTranslateXY [
	| transform |
	transform := self newTransform.
	transform translateX: 0 Y: 0.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 0@0.
	self assert: (transform transform: 1@2) equals: 1@2.
	self assert: (transform transform: -3 @ -4) equals: -3 @ -4.
	self assert: (transform transform: 1 @ -2) equals: 1 @ -2.
	self assert: (transform transform: -2 @ -5) equals: -2 	@ -5.

	transform translateX: 4 Y: -3.
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 4 @ -3.
	self assert: (transform transform: 1@2) equals: 5 @ -1.
	self assert: (transform transform: -3 @ -4) equals: 1 @ -7.
	self assert: (transform transform: 1 @ -2) equals: 5 @ -5.
	self assert: (transform transform: -2 @ -5) equals: 2 @ -8.

	transform translateX: -3 Y: 5.
	"Translation: (4 @ -3) + (-3 @ 5) = 1@2 "
	self assert: transform isAffineTransform.
	self assert: (transform transform: 0@0) equals: 1@2.
	self assert: (transform transform: 1@2) equals: 2@4.
	self assert: (transform transform: -3 @ -4) equals: -2 @ -2.
	self assert: (transform transform: 1 @ -2) equals: 2 @ 0.
	self assert: (transform transform: -2 @ -5) equals: -1 	@ -3
]

{ #category : #tests }
AthensTransformTest >> transformClass [
	self subclassResponsibility
]
