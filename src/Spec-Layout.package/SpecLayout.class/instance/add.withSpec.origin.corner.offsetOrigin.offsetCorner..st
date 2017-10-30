commands
add: modelGetter withSpec: aSymbol origin: originPoint corner: cornerPoint offsetOrigin: ooPoint offsetCorner: ocPoint

	commands add: (SpecLayoutAddWithSpec 
							subwidget: modelGetter 
							spec: aSymbol 
							layoutFrame: ((originPoint corner: cornerPoint) asSpecLayoutFrame 
												topLeftOffset: ooPoint;
												bottomRightOffset: ocPoint))