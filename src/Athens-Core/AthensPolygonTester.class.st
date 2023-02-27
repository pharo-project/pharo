"
I implementing a simple and efficient algorithm to test whether given point
lies withing a polygon or not.

A polygon is a collection of points 1...size.
"
Class {
	#name : #AthensPolygonTester,
	#superclass : #Object,
	#instVars : [
		'polygon'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #testing }
AthensPolygonTester >> includesPoint: aPoint [
"
Thanks to Google and Randolph Franklin i don't have to reinvent this very simple algorithm.
See [ 1 ] for details, copyrights etc.

[1] http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

"
	| inside testX testY i j size |

	inside := false.

	i := 1.
	size := polygon size.
	j := size.
	testX := aPoint x.
	testY := aPoint y.

	[ i <= size ] whileTrue: [  | pi pj |
		pi := polygon at: i.
		pj := polygon at: j.

		(((pi y > testY) ~= (pj y > testY)) and: [
			testX <
			(
				pj x - pi x
				* (testY - pi y)
				/ ( pj y - pi y )
				+ pi x
			)
			 ]) ifTrue: [ inside := inside not ].

		j := i.
		i := i + 1.
	 ].
	^ inside
]

{ #category : #accessing }
AthensPolygonTester >> polygon: poly [

	polygon := poly
]
