"
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.
"
Class {
	#name : #BalloonLineSimulation,
	#superclass : #Object,
	#instVars : [
		'start',
		'end',
		'xIncrement',
		'xDirection',
		'error',
		'errorAdjUp',
		'errorAdjDown'
	],
	#category : #'Balloon-Simulation'
}

{ #category : #computing }
BalloonLineSimulation >> computeInitialStateFrom: source with: aTransformation [
	"Compute the initial state in the receiver."
	start := (aTransformation localPointToGlobal: source start) asIntegerPoint.
	end := (aTransformation localPointToGlobal: source end) asIntegerPoint.
]

{ #category : #accessing }
BalloonLineSimulation >> end [
	^end
]

{ #category : #accessing }
BalloonLineSimulation >> end: aPoint [
	end := aPoint
]

{ #category : #accessing }
BalloonLineSimulation >> initialX [
	^start y <= end y
		ifTrue:[start x]
		ifFalse:[end x]
]

{ #category : #accessing }
BalloonLineSimulation >> initialY [
	^start y <= end y
		ifTrue:[start y]
		ifFalse:[end y]
]

{ #category : #accessing }
BalloonLineSimulation >> initialZ [
	^0 "Assume no depth given"
]

{ #category : #printing }
BalloonLineSimulation >> printOn: aStream [
	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: start;
		nextPutAll:' - ';
		print: end;
		nextPut:$)
]

{ #category : #accessing }
BalloonLineSimulation >> start [
	^start
]

{ #category : #accessing }
BalloonLineSimulation >> start: aPoint [
	start := aPoint
]

{ #category : #computing }
BalloonLineSimulation >> stepToFirstScanLineAt: yValue in: edgeTableEntry [
	"Compute the initial x value for the scan line at yValue"
	|  startX endX startY endY yDir deltaY deltaX widthX |
	(start y) <= (end y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
		yDir := 1.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
		yDir := -1.
	].

	deltaY := endY - startY.
	deltaX := endX - startX.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[^edgeTableEntry lines: 0].

	"Check if edge goes left to right"
	deltaX >= 0 ifTrue:[
		xDirection := 1.
		widthX := deltaX.
		error := 0.
	] ifFalse:[
		xDirection := -1.
		widthX := 0 - deltaX.
		error := 1 - deltaY.
	].

	"Check if edge is horizontal"
	deltaY = 0 
		ifTrue:[	xIncrement := 0.
				errorAdjUp := 0]
		ifFalse:["Check if edge is y-major"
			deltaY > widthX 
				ifTrue:[	xIncrement := 0.
						errorAdjUp := widthX]
				ifFalse:[	xIncrement := (widthX // deltaY) * xDirection.
						errorAdjUp := widthX \\ deltaY]].

	errorAdjDown := deltaY.

	edgeTableEntry xValue: startX.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		startY to: yValue do:[:y| self stepToNextScanLineAt: y in: edgeTableEntry].
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].
]

{ #category : #computing }
BalloonLineSimulation >> stepToNextScanLineAt: yValue in: edgeTableEntry [
	"Compute the next x value for the scan line at yValue.
	This message is sent during incremental updates. 
	The yValue parameter is passed in here for edges
	that have more complicated computations,"
	| x |
	x := edgeTableEntry xValue + xIncrement.
	error := error + errorAdjUp.
	error > 0 ifTrue:[
		x := x + xDirection.
		error := error - errorAdjDown.
	].
	edgeTableEntry xValue: x.
]

{ #category : #computing }
BalloonLineSimulation >> subdivide [
	^nil
]
