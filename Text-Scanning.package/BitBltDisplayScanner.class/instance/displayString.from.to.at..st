displaying
displayString: string from: startIndex to: stopIndex at: aPoint
    | endPoint top |
   top := aPoint translateBy: 0@ font ascent.
	endPoint := font displayString: string on: bitBlt 
		from: startIndex 
		to: stopIndex 
		at: aPoint kern: kern.
		
    (emphasisCode allMask: 4)
        ifTrue: [ font displayUnderlineOn: bitBlt from: top to: endPoint ].
    (emphasisCode allMask: 16)
        ifTrue: [ font displayStrikeoutOn: bitBlt from: top to: endPoint ]