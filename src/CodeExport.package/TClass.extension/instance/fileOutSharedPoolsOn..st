*CodeExport
fileOutSharedPoolsOn: aFileStream
	"file out the shared pools of this class after prompting the user about each pool"
	| poolsToFileOut |
	poolsToFileOut := self sharedPools select: 
		[:aPool | (self shouldFileOutPool: (self environment keyAtIdentityValue: aPool))].
	poolsToFileOut do: [:aPool | self fileOutPool: aPool onFileStream: aFileStream].
	