accessing
pointCollection

	^(1 to: pointsSize * 2 by: 2) collect: [ :i | ((points at: i) / 64) @ ((points at: i + 1) / 64)]

		

		
