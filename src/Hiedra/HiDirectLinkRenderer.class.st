"
I draw links in a straight line. See comment in superclass.
"
Class {
	#name : #HiDirectLinkRenderer,
	#superclass : #HiLinkRenderer,
	#category : #Hiedra-Rendering
}

{ #category : #rendering }
HiDirectLinkRenderer >> render [
	| linkShape |
	self setPaint.
	
	linkShape := athensCanvas createPath: [:builder |		
		builder
			absolute;
			moveTo: checkpoints first;
			lineTo: checkpoints last.
		].

	athensCanvas paintMode restoreAfter: [
		athensCanvas paintMode difference.
		athensCanvas drawShape: linkShape.
	]
]
