geometry
horizontalAdvanceUpTo: aTextPosition layout: aTextLayout

	^ stop > aTextPosition spanPosition
		ifTrue: [ 0 ] 
		ifFalse: [ advance x ]
