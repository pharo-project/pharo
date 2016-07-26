rectangle functions
sideNearestTo: aPoint 
	| distToLeft distToRight distToTop distToBottom closest side |
	distToLeft := aPoint x - self left.
	distToRight := self right - aPoint x.
	distToTop := aPoint y - self top.
	distToBottom := self bottom - aPoint y.
	closest := distToLeft.
	side := #left.
	distToRight < closest ifTrue: 
		[ closest := distToRight.
		side := #right ].
	distToTop < closest ifTrue: 
		[ closest := distToTop.
		side := #top ].
	distToBottom < closest ifTrue: 
		[ closest := distToBottom.
		side := #bottom ].
	^ side
	"
 | r | r := Rectangle fromUser.
Display border: r width: 1.
[Sensor anyButtonPressed] whileFalse:
	[(r sideNearestTo: Sensor cursorPoint) , '      ' displayAt: 0@0]
"