commands
add: aSpec origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint
	
	commands add: (SpecLayoutAdd 
							subwidget: aSpec
							layoutFrame: 
								((originPoint corner: cornerPoint) asSpecLayoutFrame 
									topLeftOffset: ooPoint;
									bottomRightOffset: ocPoint ))