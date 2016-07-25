instance-creation
script: aString packageName: packageString
	^ self instanceLike: (self new initializeWithScript: aString packageName: packageString)