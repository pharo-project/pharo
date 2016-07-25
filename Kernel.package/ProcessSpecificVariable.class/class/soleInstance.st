accessing
soleInstance
	^ soleInstance ifNil: [ soleInstance := self new ]