"
SUnit Test for PointArray
"
Class {
	#name : #PointArrayTest,
	#superclass : #TestCase,
	#instVars : [
		'pointArray'
	],
	#category : #'Balloon-Tests-Collections'
}

{ #category : #running }
PointArrayTest >> setUp [ 

	super setUp.
	pointArray := PointArray with: 0@0 with: 10@3
]

{ #category : #tests }
PointArrayTest >> testAt [

	self assert: 0@0 equals: (pointArray at: 1).
	self assert: 10@3 equals: (pointArray at: 2)
	
]

{ #category : #tests }
PointArrayTest >> testAtPut [

	pointArray at: 2 put: -1 @ -1.
	self assert: 0 @ 0 equals: (pointArray at: 1).
	self assert: -1 @ -1 equals: (pointArray at: 2)
	
]

{ #category : #tests }
PointArrayTest >> testAtPutFloat [

	pointArray at: 2 put: 1.2 @ 5.5.
	self assert: 1 @ 5 equals: (pointArray at: 2)
	
]

{ #category : #tests }
PointArrayTest >> testAtPutFraction [

	pointArray at: 2 put: 1/2 @ 51/5.
	self assert: 0 @ 10 equals: (pointArray at: 2)
	
]

{ #category : #tests }
PointArrayTest >> testAtPutInt4 [

	pointArray at: 2 put: 405933 @ -405933.
	self assert: 405933@ -405933 equals: (pointArray at: 2)
	
]

{ #category : #tests }
PointArrayTest >> testAtPutLargeInteger [

	self should: [ pointArray at: 2 put: 123456789012345678901234567890@987654323456787654378989 ] raise: Error.	
]

{ #category : #tests }
PointArrayTest >> testBounds [

	self assert: (0@0 corner: 10@3) equals: pointArray bounds
		
]

{ #category : #tests }
PointArrayTest >> testDefaultElement [

	self assert: 0@0 equals: pointArray defaultElement
		
]

{ #category : #tests }
PointArrayTest >> testSize [

	self assert: 2 equals: pointArray size
		
]
