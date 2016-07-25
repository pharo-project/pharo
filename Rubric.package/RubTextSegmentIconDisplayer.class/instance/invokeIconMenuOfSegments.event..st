event handling
invokeIconMenuOfSegments: aSegmentList event: anEvent
	| menu |
	aSegmentList ifEmpty: [ ^ self ].
	menu := RubSegmentIconMenuMorph new
		defaultTarget: self;
		color: Color yellow muchLighter.
	aSegmentList
		do: [ :seg | seg addItemToMenu: menu ].
	menu hasItems
		ifFalse: [ ^ self ].
	menu borderWidth: 0.
	menu
		on: #mouseLeave
		send: #value
		to: [ 
			menu delete.
			self changed ].
	menu invokeAt: (self iconPositionOfSegment: aSegmentList first) - (1 @ 4 negated) in: self