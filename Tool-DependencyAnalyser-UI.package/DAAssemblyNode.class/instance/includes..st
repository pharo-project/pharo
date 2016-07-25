testing
includes: aPackageName
	^ (self dependencies collect: #packageName) includes: aPackageName