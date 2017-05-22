private accessing
remoteToUse
	self remote ifNotNil: [ ^ self remote ].
	self url ifNotNil: [ ^ IceRemote url: self url ]. 
	^ nil