accessing-ui
buttons
	^ #(( 'Run Selected' #runAll 'Run all selected tests' #hasRunnable )
		( 'Run Profiled' #runProfiled 'Run tests and profile' #hasRunnable )
		( 'Run Coverage' #runCoverage 'Run test and check code coverage' #hasRunnable )
		( 'Run Failures' #runFailures 'Run all failures' #hasFailures )
		( 'Run Errors' #runErrors 'Run all errors' #hasErrors )
		( 'File out results' #fileOutResults 'File out test results' #hasFailuresOrErrors))