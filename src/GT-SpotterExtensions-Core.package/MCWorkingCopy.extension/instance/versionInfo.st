*GT-SpotterExtensions-Core-private
versionInfo
	^ self ancestors 
		ifNotEmpty: [ :list | list first ] 
		ifEmpty: [ nil ]