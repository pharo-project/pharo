accessing
requestor
	^ requestor ifNil: [ requestor := self createFFICallout ]