drag and drop
acceptDroppingMorph: aMorph event: event
	"I need to more or less copy this from old pluggable list because there 
	 is much bad code around :("

	self dataSource dropItemSelector 
		ifNotNil: [ ^ super acceptDroppingMorph: aMorph event: event ].

	self model
		acceptDroppingMorph: aMorph
		event: event
		inMorph: self.
	self refresh