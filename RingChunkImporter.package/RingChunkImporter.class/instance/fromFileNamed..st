reading
fromFileNamed: aName
	
	package := RGPackage named: aName.
	self fileInFrom: aName asFileReference readStream.