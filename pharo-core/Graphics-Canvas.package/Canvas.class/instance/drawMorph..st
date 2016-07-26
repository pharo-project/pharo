drawing-general
drawMorph: aMorph
	"Draw the receiver morph on the receiver"
	
	"Changed to improve performance. Have seen a 30% improvement."	
	(aMorph fullBounds intersects: self clipRect)
		ifFalse: [^self].
	self draw: aMorph