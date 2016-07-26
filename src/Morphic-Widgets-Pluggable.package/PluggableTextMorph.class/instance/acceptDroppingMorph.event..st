layout
acceptDroppingMorph: aMorph event: evt 
	"This message is sent when a morph is dropped onto a morph that has     
	agreed to accept the dropped morph by responding 'true' to the     
	wantsDroppedMorph:Event: message. The default implementation just     
	adds the given morph to the receiver."
	"Here we let the model do its work."

	self model
		acceptDroppingMorph: aMorph
		event: evt
		inMorph: self.

