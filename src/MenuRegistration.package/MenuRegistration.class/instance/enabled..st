spec accessing
enabled: aBooleanOrABlock

	aBooleanOrABlock == true
		ifTrue: [ 
			self spec enabled: true.
			^ self ].
	aBooleanOrABlock == false
		ifTrue: [ 
			self spec enabled: false.
			^ self ].
	self spec enabled: (aBooleanOrABlock cull: self model).