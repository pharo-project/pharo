running C program
executeOSShellCommand: aCommandString
	"To perform the OS command, this method simply does a system() call via FFI. "
	self flag: #fixIt.
	"This won't work for Windows as it doesn't have system() function."
	self system: aCommandString