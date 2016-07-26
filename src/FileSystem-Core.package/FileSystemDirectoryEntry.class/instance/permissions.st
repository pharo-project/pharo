accessing
permissions 
	^self posixPermissions
		ifNotNil: [ FileSystemPermission posixPermissions: self posixPermissions ]