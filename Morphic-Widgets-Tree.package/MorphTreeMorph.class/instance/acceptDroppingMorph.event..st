dropping/grabbing
acceptDroppingMorph: aMorph event: evt
	"This message is sent when a morph is dropped onto a morph that has agreed 
	to accept the dropped morph by responding 'true' to the wantsDroppedMorph:Event: 
	message. This implementation relay the drop to the model then release the morph
	which was candidate fro a drop"
	self model
		acceptDroppingMorph: aMorph
		event: evt
		inMorph: self.
	self resetPotentialDropMorph.
	evt hand releaseMouseFocus: self.
	Cursor normal show.
