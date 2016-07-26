accessing
packageDescriptionFromPackageEntry: packageEntry
	| filename extension packageDirectory |
	packageDirectory := self fileDirectoryOn: packageEntry.
	filename := self fileUtils current directoryName: packageDirectory.
	extension := filename
		copyFrom: (filename lastIndexOf: $.)
		to: filename size.
	^ ((self packageExtension ~= '.package'
		and:
			[ (self fileUtils
				filePathExists: 'version'
				relativeTo: packageDirectory)
				and:
					[ self fileUtils
						filePathExists: 'package'
						relativeTo: packageDirectory ] ])
		or:
			[ | dir |
			dir := self fileUtils
				directoryFromPath: self class monticelloMetaDirName
				relativeTo: packageDirectory.
			self fileUtils directoryExists: dir ])
		ifTrue: [ self class parseEntry: packageEntry extension: extension ]
		ifFalse:
			[ {(filename copyFrom: 1 to: (filename lastIndexOf: $.) - 1).
			'cypress'.
			1.
			packageEntry} ]