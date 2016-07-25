rendering
renderMorph: aMorph

	canvas pathTransform restoreAfter: [  
	"	canvas pathTransform translateX: 0 Y: aMorph height negated. "
		
		aMorph currentPosition: (canvas pathTransform transform: 0@0).
	"	canvas fullDrawMorph: aMorph."
	]