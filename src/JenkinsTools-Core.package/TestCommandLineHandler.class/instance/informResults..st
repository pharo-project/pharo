private
informResults: results
	self stdout 
		<< ('{1} run, {2} passes, {3} failures, {4} errors.'
			format: {
				results sum: #suiteTotal.
				results sum: #suitePassing.
				results sum: #suiteFailures.
				results sum: #suiteErrors })
		<< OSPlatform current lineEnding