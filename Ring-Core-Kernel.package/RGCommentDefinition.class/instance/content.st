accessing
content

	self isActive 
		ifTrue: [ ^ self realClass organization classComment ].
	self isHistorical
		ifTrue: [ ^ self contentAtPointer ifNil:[ self realClass ifNil:[ content ] ifNotNil:[ :rc| rc organization classComment ] ] ].	
	^ content