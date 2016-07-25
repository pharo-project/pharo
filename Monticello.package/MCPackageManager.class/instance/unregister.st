operations
unregister
	self class registry 
		removeKey: package
		ifAbsent: [ ^ self ].
	self class changed: (Array with: #unregistered with: package).
	self announcer announce: (MCWorkingCopyDeleted workingCopy: self package: package)