building
versionWithSnapshot: aSnapshot name: aString
	| info |
	info := self mockVersionInfo: aString. 
	^ MCVersion 
		package: (MCPackage new name: aString)
		info: info
		snapshot: aSnapshot