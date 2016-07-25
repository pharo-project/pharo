calculating
updateInfo

 	lineTopOffset := 0.
	
	layout lines do: [:each |
		(each interval contains: textPosition) ifTrue: [ 
			"found the line"
			line := each.
			horizontalOffset := line horizontalOffsetFor: textPosition .
			command := line commandForPosition: textPosition ifOutside: nil.
			^ self
		].
		lineTopOffset := lineTopOffset + each height.
	].

	horizontalOffset isNil ifTrue:[ self error: 'should not get here!'].