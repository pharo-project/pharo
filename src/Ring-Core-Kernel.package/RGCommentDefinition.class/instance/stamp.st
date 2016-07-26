accessing
stamp
	"Retrieves the user-alias + timestamp associated to the receiver (if exists)"
	
	self isActive 
		ifTrue: [ ^ self realClass organization commentStamp ].
	self isHistorical
		ifTrue: [ ^ self stampAtPointer ifNil:[ self realClass ifNil:[ stamp ] ifNotNil:[ :rc| rc organization commentStamp ] ] ].	
	^ stamp