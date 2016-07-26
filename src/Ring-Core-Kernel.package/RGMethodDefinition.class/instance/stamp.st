accessing
stamp

	self isActive 
		ifTrue: [ ^ self compiledMethod timeStamp ].
	self isHistorical
		ifTrue: [ ^ self stampAtPointer ifNil:[ self compiledMethod ifNil:[ stamp ] ifNotNil:[ :cm| cm timeStamp ] ] ].
	^ stamp