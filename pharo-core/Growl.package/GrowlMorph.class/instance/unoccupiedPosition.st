position
unoccupiedPosition

	self class position = #bottomLeft
		ifTrue: [ ^ self unoccupiedPositionBottomLeft ].
	self class position = #topRight
		ifTrue: [ ^ self unoccupiedPositionTopRight ].
	self class position = #bottomRight
		ifTrue: [ ^ self unoccupiedPositionBottomRight ].
	self class position = #topLeft
		ifTrue: [ ^ self unoccupiedPositionTopLeft ].
	^ 0@0	
	