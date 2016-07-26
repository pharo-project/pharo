point functions
bearingToPoint: anotherPoint
    "Return the bearing, in degrees, from the receiver to anotherPoint."

    | deltaX deltaY  |
    deltaX := anotherPoint x -  x.
    deltaY := anotherPoint y - y.
    deltaX abs < 0.001
        ifTrue: [ ^ deltaY > 0 ifTrue: [ 180 ] ifFalse: [ 0 ]].
    ^ ((deltaX >= 0 ifTrue: [90] ifFalse: [270])
            - ((deltaY / deltaX) arcTan negated radiansToDegrees)) rounded
