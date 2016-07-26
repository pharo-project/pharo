initialize - release
initializeAllMyChildren
	"Use a Dictionary if process identifiers are unique. On Windows, the
	process ID is not unique, so use an OrderedCollection instead."

	^ childProcessList := OrderedCollection new
