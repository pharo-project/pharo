signaling
signalDependency: dependencyName on: aPackageName
	^ self basicNew 
		initializeWithDependency: dependencyName on: aPackageName;
		signal;
		yourself