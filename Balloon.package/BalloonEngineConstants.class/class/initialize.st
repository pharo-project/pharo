pool definition
initialize
	"BalloonEngineConstants initialize"
	self initStateConstants.
	self initWorkBufferConstants.
	self initPrimitiveConstants.
	self initEdgeConstants.
	self initFillConstants.
	self initializeInstVarNames: BalloonEngine prefixedBy: 'BE'.
	self initializeInstVarNames: BalloonEdgeData prefixedBy: 'ET'.
	self initializeInstVarNames: BalloonFillData prefixedBy: 'FT'.