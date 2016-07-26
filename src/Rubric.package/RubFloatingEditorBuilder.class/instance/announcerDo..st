accessing
announcerDo: aBlock
	announcer ifNil: [ ^ self ].
	aBlock cull: announcer