testing
hasMonticelloMetadata
    ^ (self fileUtils
		filePathExists: 'version'
		relativeTo:
			(self fileUtils
				directoryFromPath: MCFileTreeStCypressWriter monticelloMetaDirName
				relativeTo: packageDirectory))
		or:
			[ (self fileUtils
				filePathExists: 'version'
				relativeTo: packageDirectory)
				and:
					[ self fileUtils
						filePathExists: 'package'
						relativeTo: packageDirectory ] ]
