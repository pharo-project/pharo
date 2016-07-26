private
evaluateSelection
	^ OpalCompiler new
		source: morph editor selection readStream;
		"Note subtle difference versus  (morph editor selectionAsStream). 
		The later does not answer the same contents and would raise a SyntaxErrorNotification with wrong sub-selection"
		failBlock: [^failure];
		evaluate