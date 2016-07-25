mc-manipulation
mainRepo

	^ mainRepo ifNil: [ mainRepo :=
		self package mcPackage workingCopy repositoryGroup repositories detect: [ :repo |
			repo location matchesRegexIgnoringCase:
				'.+smalltalkhub\.com.+', self projectName, '.+' ] ]