accessing
logsDirectory
	self flag: #todo.	"The base locator could have a wrong fileSystem"

	^ self current sessionStore baseLocator asFileReference