private
mapPermissionsFromUnix: unixPerms
	"Take Unix permissions (e.g. 8r40755 for a directory) and put them in the high 16 bits. The low 16 bits are for DOS permissions"
	
	^ unixPerms bitShift: 16.