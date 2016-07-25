testing
removedPackagesIncludes: seenPackage

	^ self packagesDiffRemoved detect: [ :each | each content = seenPackage ]
			ifFound: [ true ]
			ifNone: [ false ]
