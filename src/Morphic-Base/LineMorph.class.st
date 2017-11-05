"
This is really only a shell for creating single-segment straight-line Shapes.
"
Class {
	#name : #LineMorph,
	#superclass : #PolygonMorph,
	#category : #Morphic-Base-Basic
}

{ #category : #'instance creation' }
LineMorph class >> from: startPoint to: endPoint color: lineColor width: lineWidth [

	^ self vertices: {startPoint. endPoint}
			color: Color black borderWidth: lineWidth borderColor: lineColor
]

{ #category : #'instance creation' }
LineMorph class >> new [
	^ self from: 0@0 to: 50@50 color: Color black width: 2
]
