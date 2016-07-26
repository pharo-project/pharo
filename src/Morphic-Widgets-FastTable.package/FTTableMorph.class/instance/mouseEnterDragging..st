event handling
mouseEnterDragging: event
	self enabled ifFalse: [ ^ self ].
	(event hand hasSubmorphs and: [ self dropEnabled ]) 
		ifFalse: [ "no d&d" ^ super mouseEnterDragging: event ].	