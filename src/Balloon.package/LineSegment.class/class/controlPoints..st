instance creation
controlPoints: anArray
	"Create a new instance of the receiver from the given control points"
	anArray size = 2 ifTrue:[^LineSegment new initializeFrom: anArray].
	anArray size = 3 ifTrue:[^Bezier2Segment new initializeFrom: anArray].
	anArray size = 4 ifTrue:[^Bezier3Segment new initializeFrom: anArray].
	self error:'Unsupported'.