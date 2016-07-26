accessing
unixFileAttributes: perms
	"See #unixFileAttributes comment"
	
	| oldPerms newPerms |
	oldPerms := self mapPermissionsToUnix: externalFileAttributes.
	newPerms :=  self isDirectory
			ifTrue: [ (perms bitAnd: FileAttrib bitInvert) bitOr: DirectoryAttrib ]
			ifFalse: [ (perms bitAnd: DirectoryAttrib bitInvert) bitOr: FileAttrib ].
	externalFileAttributes := self mapPermissionsFromUnix: newPerms.
	^oldPerms.