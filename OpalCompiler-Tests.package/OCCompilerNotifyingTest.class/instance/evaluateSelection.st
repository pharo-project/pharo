private
evaluateSelection
	^ OpalCompiler new
		source: morph editor selectionAsStream;
		requestor: morph editor;
		failBlock: [^failure];
		evaluate

	