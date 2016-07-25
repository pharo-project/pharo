construction
addSpacer
	"Add a new spacer to the receiver. 
	 
	Spacer are objects that try to use as much space as they can"
	self
		addMorphBack: (AlignmentMorph newSpacer: Color transparent)