test readonly vars
testAssignmentOfGlobalVarBinding
	interactive := true.
	self initializeErrorMessage.
	text := 'temp | | OCCompilerTestTestVar := 2'.
	self initializeErrorMessage.
	OpalCompiler new
		source: text;
		class: MockForCompilation;
		requestor: self;
		failBlock: [ self fail.
			^ nil ];
		translate