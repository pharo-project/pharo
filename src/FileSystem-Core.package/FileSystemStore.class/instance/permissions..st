public
permissions: aPath
	self nodeAt: aPath 
		ifPresent: [ :entry | ^ FileSystemPermission posixPermissions: (self basicPosixPermissions: entry) ] 
		ifAbsent: [ ^ FileSystemPermission default ].
	