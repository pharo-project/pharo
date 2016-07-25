layout
center: targetMorph with: anotherMorph
	"Change the receiver to center the targetMorph with anotherMorph. 
	Note the targetMorph is the morph on which the receiver will be applied.
	anotherMorph is often the morph containing the targetMorph. 
	"
	
	self topOffset: (anotherMorph height - targetMorph height) // 2.
	self leftOffset: (anotherMorph width - targetMorph width) // 2.
	