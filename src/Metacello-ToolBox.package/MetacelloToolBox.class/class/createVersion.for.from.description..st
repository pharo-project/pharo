scripts
createVersion: targetVersionString for: configurationBasename from: sourceVersionString description: aString
	"Create a new version based on the specifications in the given version. 
	 If the new version string contains 'baseline', then create a new baseline version.
	 If the given version is a baseline, then create a new version based on the given baseline.
	 If the given version is a non-baseline version, then create a new version based on the given version."

	<apiDocumentation>
	| toolbox sourceVersion |
	toolbox := self configurationNamed: configurationBasename.
	sourceVersion := toolbox project version: sourceVersionString.
	^ (targetVersionString indexOfSubCollection: 'baseline') > 0
		ifTrue: [ 
			"create baseline version from baseline version"
			sourceVersion blessing ~~ #baseline
				ifTrue: [ 
					self
						error:
							'Cannot create a baseline version ' , sourceVersionString printString , ' from a non-baseline version '
								, targetVersionString printString ].
			self
				createBaseline: targetVersionString
				for: configurationBasename
				from: sourceVersionString
				description: aString ]
		ifFalse: [ 
			sourceVersion blessing == #baseline
				ifTrue: [ 
					"create development version version from baseline version"
					self
						createDevelopment: targetVersionString
						for: configurationBasename
						importFromBaseline: sourceVersion versionString
						description: aString ]
				ifFalse: [ 
					"create development version version from development version"
					self
						createDevelopment: targetVersionString
						for: configurationBasename
						from: sourceVersion versionString
						description: aString ] ]