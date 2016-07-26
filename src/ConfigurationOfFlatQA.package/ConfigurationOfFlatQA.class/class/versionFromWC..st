mc-manipulation
versionFromWC: aWorkingCopy

	| versName repo |

	versName := aWorkingCopy versionInfo name.
	repo := aWorkingCopy repositoryGroup repositories detect: [ :r |
		r includesVersionNamed: versName ].
	^ repo versionFrom: versName