testing
canHandleUrl: aRepositoryUrl
	"Very simplistic implementation that does not cover all cases"
	^ (aRepositoryUrl beginsWith: 'git@' )
		or: [aRepositoryUrl beginsWith: 'ssh://']