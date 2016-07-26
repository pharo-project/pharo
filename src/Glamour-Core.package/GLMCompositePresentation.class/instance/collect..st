accessing
collect: aBlock
	^ self copy presentations: (self presentations collect: aBlock ); yourself