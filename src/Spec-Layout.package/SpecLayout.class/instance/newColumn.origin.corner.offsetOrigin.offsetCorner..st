commands
newColumn: aBlock origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint
	| block |
	
	block := aBlock.
	block isBlock ifFalse: [ block := [:c | c add: aBlock ] ].
	
	commands add: (SpecLayoutAddColumn
							block: block
							layoutFrame: ((originPoint corner: cornerPoint) asLayoutFrame
										topLeftOffset: ooPoint;
										bottomRightOffset: ocPoint)).