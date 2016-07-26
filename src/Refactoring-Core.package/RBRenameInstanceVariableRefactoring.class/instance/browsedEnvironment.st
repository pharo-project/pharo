refactoring
browsedEnvironment
	^ browsedEnvironment ifNil: [ browsedEnvironment := RBBrowserEnvironment new ]