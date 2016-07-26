dropping/grabbing
wantsDroppedMorph: aMorph event: evt
	
	self visible ifFalse: [^ false].  "will be a call to #hidden again very soon"
	self dropEnabled ifFalse: [^ false].
	^ true