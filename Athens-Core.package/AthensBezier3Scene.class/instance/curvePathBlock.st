as yet unclassified
curvePathBlock
	^ [ :builder |
		builder 
			absolute;
			moveTo: pt1;
			curveVia: pt2 and: pt3 to: pt4
	 ].
