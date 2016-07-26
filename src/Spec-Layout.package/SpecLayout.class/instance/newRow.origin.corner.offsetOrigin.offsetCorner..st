commands
newRow: aBlock origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint
	| block |
	
	block := aBlock.
	block isBlock ifFalse: [ block := [:r | r add: aBlock ] ].
	
	commands add: (SpecLayoutAddRow
							block: block
							layoutFrame: ((originPoint corner: cornerPoint) asLayoutFrame 
										topLeftOffset: ooPoint;
										bottomRightOffset: ocPoint)).