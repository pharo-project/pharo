copying
errorDontCopy
	"copying a structure, involved in concurrent operations is a bad idea"
	^ self error: 'Copying not available'