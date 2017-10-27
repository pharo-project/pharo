accessing
currentWorkingDirectoryPathWithBuffer: aByteString
	<primitive: 'primitiveGetCurrentWorkingDirectory' module: '' error: ec>
	
	^ (self getPwdViaFFI: aByteString size: aByteString size) 
		ifNil:[ self error:'Insufficient buffer size to read current working directory: ' , aByteString size asString]