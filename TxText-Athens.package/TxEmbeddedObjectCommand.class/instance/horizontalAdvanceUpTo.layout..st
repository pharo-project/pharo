geometry
horizontalAdvanceUpTo: aTextPosition layout: aTextLayout

	^ aTextPosition spanPosition = 0 
		ifTrue:[ 0 ]
		ifFalse: [ self morph width ]