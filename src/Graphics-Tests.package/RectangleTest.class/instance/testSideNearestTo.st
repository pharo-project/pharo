tests - rectangle functions
testSideNearestTo
	"rectangular area of screen implies that negalive coordinates not allowed"
	| rectangle |
	rectangle := 5 @ 5 corner: 45 @ 45.
	self assert: #left equals: (rectangle sideNearestTo: 4 @ 34).
	self assert: #right equals: (rectangle sideNearestTo: 60 @ 21).
	self assert: #top equals: (rectangle sideNearestTo: 25 @ 1).
	self assert: #bottom equals: (rectangle sideNearestTo: 23 @ 65)