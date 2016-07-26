private
mapPermissionsToUnix: zipPerms
	"Extract Unix permissions (the high 16 bits) from zip permissions. The low 16 bits are for DOS permissions"
	
	^ zipPerms bitShift: -16.