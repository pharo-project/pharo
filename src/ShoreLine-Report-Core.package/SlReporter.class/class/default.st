instance creation
default

	^ self basicNew initialize
		path: self defaultPath;
		yourself