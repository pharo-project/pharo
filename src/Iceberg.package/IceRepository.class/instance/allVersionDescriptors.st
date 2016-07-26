versions handling
allVersionDescriptors
	^ versionDescriptors ifNil: [ versionDescriptors := self repository readableFileNames ]
