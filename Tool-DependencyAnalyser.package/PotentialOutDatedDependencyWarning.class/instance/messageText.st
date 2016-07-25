accessing
messageText
	^ messageText 
		ifNil: [ package , ': ' , dependency , ' dependency declared in the package Manifest as manuallyResolvedDependencies not detected as a dependency!' ]