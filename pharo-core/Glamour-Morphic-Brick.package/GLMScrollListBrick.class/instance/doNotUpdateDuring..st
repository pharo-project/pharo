layouting
doNotUpdateDuring: aBlock 

	self isUpdating ifTrue: [ ^ self ].
	isUpdating := true.
	aBlock cull: self.
	isUpdating := false.