instance creation
morphicConfigure
	^ (UIManager default chooseDirectoryFrom: self defaultDirectoryName asFileReference) ifNotNil:
		[:directory |
		self new directory: directory]