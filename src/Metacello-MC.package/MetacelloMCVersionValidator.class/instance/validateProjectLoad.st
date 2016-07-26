loading
validateProjectLoad
	| issues project versions currentVersion collectedIssues |
	(issues := self validateProject select: [ :issue | issue isError ]) notEmpty
		ifTrue: [ ^ issues ].
	project := self configurationClass project.
	collectedIssues := issues.
	versions := project versions
		select: [ :version | 
			validationReport := nil.
			version blessing ~~ #broken
				and: [ 
					version blessing ~~ #baseline
						and: [ (issues := (self validateProject: project version: version versionString) select: [ :issue | issue isCritical ]) isEmpty ] ] ].
	validationReport := collectedIssues.
	versions isEmpty
		ifTrue: [ 
			self
				recordValidationCriticalWarning: 'No non #baseline versions  available  in ' , self configurationClass name asString
				callSite: #validateProjectLoad
				reasonCode: #noLoadableVersions.
			^ self validationReport ].
	(currentVersion := project currentVersion) ~~ nil
		ifTrue: [ 
			| index |
			index := versions indexOf: currentVersion.
			versions := versions copyFrom: index to: versions size ].
	versions do: [ :version | self validateCleanLoadAndTestsForVersion: version loads: #('ALL') ].
	^ self validationReport