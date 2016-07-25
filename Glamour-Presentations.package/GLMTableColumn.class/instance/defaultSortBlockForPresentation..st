accessing
defaultSortBlockForPresentation: aGlamourPresentation
	
	^ [ :x :y | 		
			(aGlamourPresentation
				column: self 
				valueFor: x) asString <
			(aGlamourPresentation 
				column: self 
				valueFor: y) asString ] 